
# coding: utf-8

# In[1]:

import numpy as np
import pandas as pd
import logging


# In[2]:

def simulation(nrespondents,k,samplescheme,batchsize,itemfile,designfile,mis=0,seed=0):
    if seed>0:
        np.random.seed(seed)#for reproducibility
    if mis>0:
        data1=pd.read_csv(itemfile)
        utilsall=data1.get_values()[:,2:]
        realmean=np.mean(utilsall,0)
        topmis=np.argsort(realmean)[::-1][:mis]
    data=pd.read_csv(itemfile)#
    MDDesign =pd.read_csv(designfile)#get the files
    utilsall=data.get_values()[:,2:]#utils of bots
    [numreal,nitems]=utilsall.shape#
    design=MDDesign.get_values()[:,2:]#changes to a numpy array
    numver=max(MDDesign['Version'])#number of versions
    qperset=max(MDDesign['Set'])#questions per set
    itemsperq=design.shape[1]#items per questions
    itemsperset=np.max(design)#items per set
    people=np.random.choice([x for x in range(0,numreal)],nrespondents)#choice the bots
    totalrows=qperset*itemsperq#rows
    nb=np.zeros(nitems)
    nw=np.zeros(nitems)
    ss=samplescheme(nitems,k, batchsize, itemsperset)#create a sample sceme
    allquestions=np.zeros((nrespondents*qperset,itemsperq),dtype=np.int)
    allbest=np.zeros(nrespondents*qperset,dtype=np.int)
    allworst=np.zeros(nrespondents*qperset,dtype=np.int)
    X=np.zeros((nrespondents*qperset,nitems))
    w=np.zeros(nitems)
    numrows=nrespondents//batchsize
    betas=np.zeros((numrows,nitems))
    for iternum in range(0,nrespondents):
        util=utilsall[people[iternum],:]
        if mis>0 and iternum<=150:
            lowval=np.percentile(util,iternum/2)
            util[topmis]=lowval
        vernum=iternum%numver
        respdesign=design[qperset*vernum:(qperset)*(vernum+1)]-1
        questions=ss.get_questions(respdesign)#gets items
        bestitems,worstitems=fillsurvey(questions,util)
        for i,bitem in enumerate(bestitems):
            nb[bitem]+=1
            allbest[iternum*qperset+i]=bitem
        for i,witem in enumerate(worstitems):
            nw[witem]+=1
            allworst[iternum*qperset+i]=witem
        allquestions[iternum*qperset:(iternum+1)*qperset]=questions
        for i,oneq in enumerate(questions):
            X[iternum*qperset+i,oneq]=1
        if iternum%batchsize==(batchsize-1):
            tempX=X[:(iternum+1)*qperset]
            tempq=allquestions[:(iternum+1)*qperset]
            tempbest=allbest[:(iternum+1)*qperset]
            tempworst=allworst[:(iternum+1)*qperset]
            if iternum!=(nrespondents-1):
                ss.update(tempX,tempq,tempbest,tempworst)
            w=estimateparams(tempX,tempq,nb,nw,[])
            betas[(iternum+1)//(batchsize)-1]=w
    return betas,allquestions,allbest,allworst,people


# In[3]:

def fillsurvey(questions,util):
    qperset,itemsperq=questions.shape
    noiseb= -np.log(-np.log(np.random.rand(qperset,itemsperq)))
    noisew=  np.log(-np.log(np.random.rand(qperset,itemsperq)))
    combutilb=util[questions]+noiseb
    combutilw=util[questions]+noisew
    best=questions[range(qperset),np.argmax(combutilb,1)]
    worst=questions[range(qperset),np.argmin(combutilw,1)]
    return best,worst


# In[4]:

def estimateparams(X,questions,nb,nw,w0=[],tol=.0001,maxiter=100,hessian=0):
    tol=.001
    val=1
    [numofq,nitems]=X.shape
    if len(w0)<nitems or (np.abs(w0)>4).any():
        w=np.zeros(nitems)    
    else:
        w=w0.copy()
    iternum=0
    inplay=np.where(np.bincount(questions.flatten(),minlength=nitems)>0)[0]
    notinplay=np.where(np.bincount(questions.flatten(),minlength=nitems)==0)[0]
    try:
        while val>tol and iternum<maxiter:
            zb=np.sum(np.exp(w[questions]),1)
            zw=np.sum(np.exp(-w[questions]),1)
            Pb=((1/(zb)*X.T).T*np.exp(w))#broadcasting
            Pw=((1/(zw)*X.T).T*np.exp(-w))#broadcasting
            gradb=nb-np.sum(Pb,0)
            gradw=-nw+np.sum(Pw,0)
            grad=-(gradb+gradw)
            Hb=np.dot(Pb.T,Pb)
            Hbd=-np.sum(Pb*(1-Pb),0)
            Hb+=-np.diag(np.diag(Hb))+np.diag(Hbd)
            Hw=np.dot(Pw.T,Pw)
            Hwd=-np.sum(Pw*(1-Pw),0)
            Hw+=-np.diag(np.diag(Hw))+np.diag(Hwd)
            H=-(Hb+Hw)
            #gradl=grad[:-1]
            #Hl=H[:-1,:-1]
            gradl=grad[inplay[:-1]]
            Hl=H[inplay[:-1],:][:,inplay[:-1]]
            #Hlinv=np.linalg.pinv(Hl)
            #Hinvgradl=np.dot(Hlinv,gradl)
            Hinvgradl=np.linalg.solve(Hl,gradl)
            #w[:-1]=w[:-1]-Hinvgradl
            w[inplay[:-1]]=w[inplay[:-1]]-Hinvgradl
            val=np.dot(gradl,Hinvgradl)
            iternum+=1
    except np.linalg.linalg.LinAlgError:
        print('error')
        print(numofq)
        if hessian:
            return np.random.randn(nitems)/4,np.eye(nitems-1)
        else:
            return np.random.randn(nitems)/4
    if np.isnan(w).any():
        print('nan')
        print(numofq)
        return np.random.randn(nitems)/4
        #raise Exception('w contains nan')
    w[notinplay]=w[np.random.choice(inplay,notinplay.shape[0])]+.001
    if hessian:
        H=np.eye(nitems-1)
        for i,m in enumerate(inplay[:-1]):
            for j,n in enumerate(inplay[:-1]):
                H[m,n]=Hl[i,j]
        return w,H
    else:
        return w


# In[5]:

def bayesBootstrap(X,questions,allbest,allworst,numdraws=1):#Not currently used
    [numofq,nitems]=X.shape
    draws=np.zeros((numdraws,nitems))
    #w=estimateparamsbayes(X,questions,Xb,Xw,w0=[],weights=np.ones(numofq))
    for i in range(numdraws):
        #weights=np.random.dirichlet(np.ones(numofq))*numofq
        weights=np.random.exponential(1,numofq)
        draws[i]=estimateparamsbayes(X,questions,allbest,allworst,w0=[],weights=weights)
    return draws


# In[6]:

def estimateparamsbayes(X,questions,allbest,allworst,w0=[],weights=[],tol=.0001,maxiter=100):
    tol=.001
    val=1
    [numofq,nitems]=X.shape
    if len(w0)<nitems or (np.abs(w0)>4).any():
        w=np.zeros(nitems)    
    else:
        w=w0.copy()
    if len(weights)<numofq:
        weights=np.ones(numofq)
    iternum=0
    nb=np.bincount(allbest,weights,minlength=nitems)
    nw=np.bincount(allworst,weights,minlength=nitems)
    inplay=np.where(np.bincount(questions.flatten(),minlength=nitems)>0)[0]
    notinplay=np.where(np.bincount(questions.flatten(),minlength=nitems)==0)[0]
    try:
        while val>tol and iternum<maxiter:
            zb=np.sum(np.exp(w[questions]),1)
            zw=np.sum(np.exp(-w[questions]),1)
            Pb=((1/(zb)*X.T).T*np.exp(w))#broadcasting
            Pw=((1/(zw)*X.T).T*np.exp(-w))#broadcasting
            wPb=(Pb.T*weights).T
            wPw=(Pw.T*weights).T
            gradb=nb-np.sum(wPb,0)
            gradw=-nw+np.sum(wPw,0)
            grad=-(gradb+gradw)
            Hb=np.dot(wPb.T,Pb)
            Hbd=-np.sum(wPb*(1-Pb),0)
            Hb+=-np.diag(np.diag(Hb))+np.diag(Hbd)
            Hw=np.dot(wPw.T,Pw)
            Hwd=-np.sum(wPw*(1-Pw),0)
            Hw+=-np.diag(np.diag(Hw))+np.diag(Hwd)
            H=-(Hb+Hw)
            #gradl=grad[:-1]
            #Hl=H[:-1,:-1]
            gradl=grad[inplay[:-1]]
            Hl=H[inplay[:-1],:][:,inplay[:-1]]
            #Hlinv=np.linalg.pinv(Hl)
            #Hinvgradl=np.dot(Hlinv,gradl)
            Hinvgradl=np.linalg.solve(Hl,gradl)
            #w[:-1]=w[:-1]-Hinvgradl
            w[inplay[:-1]]=w[inplay[:-1]]-Hinvgradl
            val=np.dot(gradl,Hinvgradl)
            iternum+=1
    except np.linalg.linalg.LinAlgError:
        print('error')
        print(numofq*4/12)
        return np.random.randn(nitems)/4
    if np.isnan(w).any():
        print('nan')
        print(numofq*4/12)
        return np.random.randn(nitems)/4
        #raise Exception('w contains nan')
    w[notinplay]=w[np.random.choice(inplay,notinplay.shape[0])]+.001
    return w


# In[7]:

def fixedexpress(nitems,N):
    return np.random.choice([x for x in range(0,nitems)],N,False)


# In[8]:

def getX(numofq,nitems,allthem):#Not currently used
    X=np.zeros((numofq,nitems))
    X[range(numofq),allthem]=1
    return X


class FixedExpressClass:#each sample scheme is a class
    def __init__(self,nitems,k, bsize, itemsperset):
        self.bsize = bsize
        self.k=k
        self.itemsperset = itemsperset
        self.items=np.zeros((bsize,itemsperset),dtype=np.int)
        self.rowiter=0
        self.nitems=nitems
        for i in range(bsize):
            self.items[i]=fixedexpress(nitems,itemsperset)
        
    def update(self,X,questions,allbest,allworst):
        self.rowiter=0
        for i in range(self.bsize):
            self.items[i]=fixedexpress(self.nitems,self.itemsperset)
  
    
    def get_questions(self,design):
        items=self.items[self.rowiter]
        self.rowiter+=1
        return items[design]

class TS:#each sample scheme is a class
    def __init__(self,nitems,k, bsize, itemsperset,esp=1/4,delta=.25,thres=0):
        self.bsize = bsize
        self.k=k
        self.itemsperset = itemsperset
        self.nondif=itemsperset-int(np.round(itemsperset*esp))
        self.dif=int(np.round(itemsperset*esp))
        self.delta=delta
        self.items=np.zeros((bsize,itemsperset),dtype=np.int)
        self.rowiter=0
        self.thres=thres
        for i in range(bsize):
            self.items[i]=fixedexpress(nitems,itemsperset)
        
    def update(self,X,questions,allbest,allworst):
        self.rowiter=0
        [numofq,nitems]=X.shape
        draws=np.zeros((self.bsize,nitems))
        
        
        #if self.thres:
        #    nb=np.bincount(allbest,minlength=nitems)
        #    nw=np.bincount(allworst,minlength=nitems)
        #    w=estimateparams(X,questions,nb,nw)
        #    order=np.argsort(w)[::-1]
        #    threshold=(w[order[self.k-1]]+w[order[self.k]])/2
        

        #w=estimateparamsbayes(X,questions,Xb,Xw,w0=[],weights=np.ones(numofq))
        for i in range(self.bsize):
            weights=np.random.exponential(1,numofq)
            w=estimateparamsbayes(X,questions,allbest,allworst,weights=weights)
            if self.thres:
                order=np.argsort(w)[::-1]
                threshold=(w[order[self.k-1]]+w[order[self.k]])/2
                draws[i]=-np.abs(threshold-w)
            else:
                draws[i]=w 

        #if self.thres:
        #    draws=-np.abs(threshold-draws)
        if self.dif:
            drawsdif=np.zeros((self.bsize,nitems))
            for i in range(self.bsize):
                subsamp=int(numofq*self.delta)
                if subsamp>0:
                    sub=np.random.choice([x for x in range(0,numofq)],subsamp)
                    tempX=X[sub]
                    tempb=allbest[sub]
                    tempw=allworst[sub]
                    tempq=questions[sub]
                    weights=np.random.exponential(1,subsamp)
                    w=estimateparamsbayes(tempX,tempq,tempb,tempw,weights=weights)
                    if self.thres:
                        order=np.argsort(w)[::-1]
                        threshold=(w[order[self.k-1]]+w[order[self.k]])/2
                        drawsdif[i]=-np.abs(threshold-w)
                    else:
                       drawsdif[i]=w
                else:
                    drawsdif[i]=np.random.randn(nitems)
                #drawsdif=-np.abs(threshold-drawsdif)
            
        for i in range(self.bsize):
            if self.dif:
                start=np.argsort(draws[i])[::-1][:self.nondif]
                difsort=np.argsort(drawsdif[i])[::-1]
                diffuse=[x for x in difsort if x not in start]
                self.items[i,:self.nondif]=start
                self.items[i,self.nondif:]=diffuse[:self.dif]
            else:
                start=np.argsort(draws[i])[::-1][:self.itemsperset]
                self.items[i,:]=start
    
    def get_questions(self,design):
        items=self.items[self.rowiter]
        self.rowiter+=1
        return items[design]

class TSapprox:
    def __init__(self,nitems,k, bsize, itemsperset,esp=1/4,delta=10,thres=0):
        self.bsize = bsize
        self.k=k
        self.itemsperset = itemsperset
        self.nondif=itemsperset-int(np.round(itemsperset*esp))
        self.dif=int(np.round(itemsperset*esp))
        self.delta=delta
        self.items=np.zeros((bsize,itemsperset),dtype=np.int)
        self.rowiter=0
        self.thres=thres
        for i in range(bsize):
            self.items[i]=fixedexpress(nitems,itemsperset)
        
    def update(self,X,questions,allbest,allworst):
        self.rowiter=0
        [numofq,nitems]=X.shape
        draws=np.zeros((self.bsize,nitems))
        
        nb=np.bincount(allbest,minlength=nitems)
        nw=np.bincount(allworst,minlength=nitems)
        w,H=estimateparams(X,questions,nb,nw,hessian=1)
        cov=np.linalg.inv(H)
        draws[:,:-1]=np.random.multivariate_normal(w[:-1], cov,self.bsize)
        
        if self.thres:
            order=np.argsort(draws)[:,::-1]
            threshold=(draws[range(self.bsize),order[:,self.k-1]]+draws[range(self.bsize),order[:,self.k]])/2
            draws=-np.abs(threshold-draws.T).T

        #w=estimateparamsbayes(X,questions,Xb,Xw,w0=[],weights=np.ones(numofq))
        if self.dif:
            drawsdif=np.zeros((self.bsize,nitems))
            drawsdif[:,:-1]=np.random.multivariate_normal(w[:-1], self.delta*cov,self.bsize)
            if self.thres:
                order=np.argsort(drawsdif)[:,::-1]
                threshold=(drawsdif[range(self.bsize),order[:,self.k-1]]+drawsdif[range(self.bsize),order[:,self.k]])/2
                drawsdif=-np.abs(threshold-drawsdif.T).T
            
        for i in range(self.bsize):
            if self.dif:
                start=np.argsort(draws[i])[::-1][:self.nondif]
                difsort=np.argsort(drawsdif[i])[::-1]
                diffuse=[x for x in difsort if x not in start]
                self.items[i,:self.nondif]=start
                self.items[i,self.nondif:]=diffuse[:self.dif]
            else:
                start=np.argsort(draws[i])[::-1][:self.itemsperset]
                self.items[i,:]=start
    
    def get_questions(self,design):
        items=self.items[self.rowiter]
        self.rowiter+=1
        return items[design]

class TSapproxthres:
    def __init__(self,nitems,k, bsize, itemsperset):
        self.ts=TSapprox(nitems,k, bsize, itemsperset,thres=1)
        
    def update(self,X,questions,allbest,allworst):
        self.ts.update(X,questions,allbest,allworst)

    
    def get_questions(self,design):
        return self.ts.get_questions(design)


class greedy:
    def __init__(self,nitems,k, bsize, itemsperset,esp=1/4,thres=0):
        self.bsize = bsize
        self.k=k
        self.itemsperset = itemsperset
        self.nondif=itemsperset-int(np.round(itemsperset*esp))
        self.dif=int(np.round(itemsperset*esp))
        self.items=np.zeros((bsize,itemsperset),dtype=np.int)
        self.rowiter=0
        self.thres=thres
        for i in range(bsize):
            self.items[i]=fixedexpress(nitems,itemsperset)
        
    def update(self,X,questions,allbest,allworst):
        self.rowiter=0
        [numofq,nitems]=X.shape
        draws=np.zeros((self.bsize,nitems))
        nb=np.bincount(allbest,minlength=nitems)
        nw=np.bincount(allworst,minlength=nitems)
        w=estimateparams(X,questions,nb,nw)
        if self.thres:
            order=np.argsort(w)[::-1]
            threshold=(w[order[self.k-1]]+w[order[self.k]])/2
            start=np.argsort(-np.abs(threshold-w))[::-1][:self.nondif]
        else:
            start=np.argsort(w)[::-1][:self.nondif]
        
        
        drawsdif=np.zeros((self.bsize,self.itemsperset))
        for i in range(self.bsize):
            drawsdif[i]=fixedexpress(nitems,self.itemsperset)
            
        for i in range(self.bsize):
            difsort=drawsdif[i]
            diffuse=[x for x in difsort if x not in start]
            self.items[i,:self.nondif]=start
            self.items[i,self.nondif:]=diffuse[:self.dif]
    
    def get_questions(self,design):
        items=self.items[self.rowiter]
        self.rowiter+=1
        return items[design]


# In[12]:

class greedythres:
    def __init__(self,nitems,k, bsize, itemsperset,esp=1/4):
        self.greed=greedy(nitems,k, bsize, itemsperset,esp,1)
        
    def update(self,X,questions,allbest,allworst):
        self.greed.update(X,questions,allbest,allworst)

    
    def get_questions(self,design):
        return self.greed.get_questions(design)


# In[13]:

class TSthres:
    def __init__(self,nitems,k, bsize, itemsperset):
        self.ts=TS(nitems,k, bsize, itemsperset,thres=1)
        
    def update(self,X,questions,allbest,allworst):
        self.ts.update(X,questions,allbest,allworst)

    
    def get_questions(self,design):
        return self.ts.get_questions(design)



class TSreg:
    def __init__(self,nitems,k, bsize, itemsperset):
        self.ts=TS(nitems,k, bsize, itemsperset,esp=0)
        
    def update(self,X,questions,allbest,allworst):
        self.ts.update(X,questions,allbest,allworst)

    
    def get_questions(self,design):
        return self.ts.get_questions(design)

class TS24:
    def __init__(self,nitems,k, bsize, itemsperset):
        self.ts=TS(nitems,k, bsize, itemsperset,esp=1/2,delta=.25)
        
    def update(self,X,questions,allbest,allworst):
        self.ts.update(X,questions,allbest,allworst)

    
    def get_questions(self,design):
        return self.ts.get_questions(design)

class TS42:
    def __init__(self,nitems,k, bsize, itemsperset):
        self.ts=TS(nitems,k, bsize, itemsperset,esp=1/4,delta=.5)
        
    def update(self,X,questions,allbest,allworst):
        self.ts.update(X,questions,allbest,allworst)

    
    def get_questions(self,design):
        return self.ts.get_questions(design)

class TS22:
    def __init__(self,nitems,k, bsize, itemsperset):
        self.ts=TS(nitems,k, bsize, itemsperset,esp=1/2,delta=.5)
        
    def update(self,X,questions,allbest,allworst):
        self.ts.update(X,questions,allbest,allworst)

    
    def get_questions(self,design):
        return self.ts.get_questions(design)

class TS20:
    def __init__(self,nitems,k, bsize, itemsperset):
        self.ts=TS(nitems,k, bsize, itemsperset,esp=1/2,delta=0)
        
    def update(self,X,questions,allbest,allworst):
        self.ts.update(X,questions,allbest,allworst)

    
    def get_questions(self,design):
        return self.ts.get_questions(design)

class TS40:
    def __init__(self,nitems,k, bsize, itemsperset):
        self.ts=TS(nitems,k, bsize, itemsperset,esp=1/4,delta=0)
        
    def update(self,X,questions,allbest,allworst):
        self.ts.update(X,questions,allbest,allworst)

    
    def get_questions(self,design):
        return self.ts.get_questions(design)

class TS12:
    def __init__(self,nitems,k, bsize, itemsperset):
        self.ts=TS(nitems,k, bsize, itemsperset,esp=1,delta=.5)
        
    def update(self,X,questions,allbest,allworst):
        self.ts.update(X,questions,allbest,allworst)

    
    def get_questions(self,design):
        return self.ts.get_questions(design)

class TS14:
    def __init__(self,nitems,k, bsize, itemsperset):
        self.ts=TS(nitems,k, bsize, itemsperset,esp=1,delta=.25)
        
    def update(self,X,questions,allbest,allworst):
        self.ts.update(X,questions,allbest,allworst)

    
    def get_questions(self,design):
        return self.ts.get_questions(design)

class TSregthres:
    def __init__(self,nitems,k, bsize, itemsperset):
        self.ts=TS(nitems,k, bsize, itemsperset,esp=0,thres=1)
        
    def update(self,X,questions,allbest,allworst):
        self.ts.update(X,questions,allbest,allworst)

    def get_questions(self,design):
        return self.ts.get_questions(design)


class TSkdouble:
    def __init__(self,nitems,k, bsize, itemsperset,esp=1/4,delta=.25):
        self.bsize = bsize
        self.k=k
        self.itemsperset = itemsperset
        self.nondif=k-int(np.round(k*esp))
        self.dif=int(np.round(k*esp))
        self.delta=delta
        self.items=np.zeros((bsize,itemsperset),dtype=np.int)
        self.rowiter=0
        for i in range(bsize):
            self.items[i]=fixedexpress(nitems,itemsperset)
        
    def update(self,X,questions,allbest,allworst):
        self.rowiter=0
        [numofq,nitems]=X.shape
        draws=np.zeros((self.bsize//2,nitems))
        
        

        #w=estimateparamsbayes(X,questions,Xb,Xw,w0=[],weights=np.ones(numofq))
        for i in range(self.bsize//2):
            weights=np.random.exponential(1,numofq)
            draws[i]=estimateparamsbayes(X,questions,allbest,allworst,weights=weights)

        if self.dif:
            drawsdif=np.zeros((self.bsize//2,nitems))
            for i in range(self.bsize//2):
                subsamp=int(numofq*self.delta)
                sub=np.random.choice([x for x in range(0,numofq)],subsamp)
                tempX=X[sub]
                tempb=allbest[sub]
                tempw=allworst[sub]
                tempq=questions[sub]
                weights=np.random.exponential(1,subsamp)
                drawsdif[i]=estimateparamsbayes(tempX,tempq,tempb,tempw,weights=weights)

            
        for i in range(self.bsize//2):
            if self.dif:
                start=np.argsort(draws[i])[::-1][:self.nondif]
                difsort=np.argsort(drawsdif[i])[::-1]
                diffuse=[x for x in difsort if x not in start]
                self.items[2*i,:self.nondif//2]=start[::2]
                self.items[2*i,self.nondif//2:]=diffuse[:self.dif:2]
                self.items[2*i+1,:self.nondif//2]=start[1::2]
                self.items[2*i+1,self.nondif//2:]=diffuse[1:self.dif+1:2]
            else:
                start=np.argsort(draws[i])[::-1][:self.itemsperset*2]
                self.items[2*i,:]=start[::2]
                self.items[2*i+1,:]=start[1::2]
    
    def get_questions(self,design):
        items=self.items[self.rowiter]
        self.rowiter+=1
        return items[design]

class misclassMin:
    def __init__(self,nitems,k, bsize, itemsperset,N=100):
        self.bsize = bsize
        self.k=k
        self.itemsperset = itemsperset
        self.items=np.zeros((bsize,itemsperset),dtype=np.int)
        self.N=N
        self.rowiter=0
        for i in range(bsize):
            self.items[i]=fixedexpress(nitems,itemsperset)
        
    def update(self,X,questions,allbest,allworst):
        self.rowiter=0
        [numofq,nitems]=X.shape
        draws=np.zeros((self.N,nitems))
        nb=np.bincount(allbest,minlength=nitems)
        nw=np.bincount(allworst,minlength=nitems)
        w=estimateparams(X,questions,nb,nw)
        
        for i in range(self.N):
            weights=np.random.exponential(1,numofq)
            draws[i]=estimateparamsbayes(X,questions,allbest,allworst,weights=weights)
        score=np.bincount(np.argsort(draws,1)[:,::-1][:,:self.k].flatten(),
                          minlength=nitems)/self.N
        order=np.argsort(w)[::-1]
        score[order[:self.k]]=1-score[order[:self.k]]
        var=.1/(np.bincount(questions.flatten(),minlength=nitems)+.01)
        zeros=np.zeros(nitems)
        
        for i in range(self.bsize):
            newscore=score+np.random.normal(zeros,np.sqrt(var))
            start=np.argsort(newscore)[::-1][:self.itemsperset]
            self.items[i]=start
    
    def get_questions(self,design):
        items=self.items[self.rowiter]
        self.rowiter+=1
        return items[design]


class misclassMinapprox:
    def __init__(self,nitems,k, bsize, itemsperset,N=100):
        self.bsize = bsize
        self.k=k
        self.itemsperset = itemsperset
        self.items=np.zeros((bsize,itemsperset),dtype=np.int)
        self.N=N
        self.rowiter=0
        for i in range(bsize):
            self.items[i]=fixedexpress(nitems,itemsperset)
        
    def update(self,X,questions,allbest,allworst):
        self.rowiter=0
        [numofq,nitems]=X.shape
        draws=np.zeros((self.N,nitems))
        nb=np.bincount(allbest,minlength=nitems)
        nw=np.bincount(allworst,minlength=nitems)
        
        w,H=estimateparams(X,questions,nb,nw,hessian=1)
        cov=np.linalg.inv(H)
        draws[:,:-1]=np.random.multivariate_normal(w[:-1], cov,self.N)
        

        score=np.bincount(np.argsort(draws,1)[:,::-1][:,:self.k].flatten(),
                          minlength=nitems)/self.N
        order=np.argsort(w)[::-1]
        score[order[:self.k]]=1-score[order[:self.k]]
        var=.5/(np.bincount(questions.flatten(),minlength=nitems)+.01)
        zeros=np.zeros(nitems)
        
        for i in range(self.bsize):
            newscore=score+np.random.normal(zeros,np.sqrt(var))
            start=np.argsort(newscore)[::-1][:self.itemsperset]
            self.items[i]=start
    
    def get_questions(self,design):
        items=self.items[self.rowiter]
        self.rowiter+=1
        return items[design]

class greatUncert:
    def __init__(self,nitems,k, bsize, itemsperset,N=100):
        self.bsize = bsize
        self.k=k
        self.itemsperset = itemsperset
        self.items=np.zeros((bsize,itemsperset),dtype=np.int)
        self.N=N
        self.rowiter=0
        for i in range(bsize):
            self.items[i]=fixedexpress(nitems,itemsperset)
        
    def update(self,X,questions,allbest,allworst):
        self.rowiter=0
        [numofq,nitems]=X.shape
        draws=np.zeros((self.N,nitems))
        
        for i in range(self.N):
            weights=np.random.exponential(1,numofq)
            draws[i]=estimateparamsbayes(X,questions,allbest,allworst,weights=weights)
        score=np.bincount(np.argsort(draws,1)[:,::-1][:,:self.k].flatten(),
                          minlength=nitems)/self.N
        score=-np.abs(.5-score)
        
        var=.5/(np.bincount(questions.flatten(),minlength=nitems)+.01)
        zeros=np.zeros(nitems)
        for i in range(self.bsize):
            newscore=score+np.random.normal(zeros,np.sqrt(var))
            start=np.argsort(newscore)[::-1][:self.itemsperset]
            self.items[i]=start
    
    def get_questions(self,design):
        items=self.items[self.rowiter]
        self.rowiter+=1
        
        return items[design]

class greedynightmare:
    def __init__(self,nitems,k, bsize, itemsperset,esp=1/4,thres=0):
        self.bsize = bsize
        self.k=k
        self.itemsperset = itemsperset
        self.nondif=itemsperset-int(np.round(itemsperset*esp))
        self.dif=int(np.round(itemsperset*esp))
        self.items=np.zeros((bsize,itemsperset),dtype=np.int)
        self.rowiter=0
        self.thres=thres
        self.iteration=1
        self.w=np.zeros(nitems)
        for i in range(bsize):
            self.items[i]=fixedexpress(nitems,itemsperset)
        
    def update(self,X,questions,allbest,allworst):
        w=self.w
        self.rowiter=0
        [numofq,nitems]=X.shape
        draws=np.zeros((self.bsize,nitems))
        num=self.bsize*12
        nb=np.bincount(allbest[-num:],minlength=nitems)
        nw=np.bincount(allworst[-num:],minlength=nitems)
        zb=np.sum(np.exp(w[questions[-num:]]),1)
        zw=np.sum(np.exp(-w[questions[-num:]]),1)
        Pb=((1/zb*X[-num:].T).T*np.exp(w))#broadcasting
        Pw=((1/zw*X[-num:].T).T*np.exp(-w))#broadcasting
        gradb=nb-np.sum(Pb,0)
        gradw=-nw+np.sum(Pw,0)
        grad=-(gradb+gradw)
        w[:-1]=w[:-1]-1/(2*self.iteration)*grad[:-1]
        self.iteration+=1
        self.w=w
        if self.thres:
            order=np.argsort(w)[::-1]
            threshold=(w[order[self.k-1]]+w[order[self.k]])/2
            start=np.argsort(-np.abs(threshold-w))[::-1][:self.nondif]
        else:
            start=np.argsort(w)[::-1][:self.nondif]
        
        
        drawsdif=np.zeros((self.bsize,self.itemsperset))
        for i in range(self.bsize):
            drawsdif[i]=fixedexpress(nitems,self.itemsperset)
            
        for i in range(self.bsize):
            difsort=drawsdif[i]
            diffuse=[x for x in difsort if x not in start]
            self.items[i,:self.nondif]=start
            self.items[i,self.nondif:]=diffuse[:self.dif]
    
    def get_questions(self,design):
        items=self.items[self.rowiter]
        self.rowiter+=1
        return items[design]

class winapprox:
    def __init__(self,nitems,k, bsize, itemsperset,esp=1/5,thres=0):
        self.bsize = bsize
        self.k=k
        self.itemsperset = itemsperset
        self.nondif=itemsperset-int(np.round(itemsperset*esp))
        self.dif=int(np.round(itemsperset*esp))
        self.items=np.zeros((bsize,itemsperset),dtype=np.int)
        self.rowiter=0
        self.thres=thres
        for i in range(bsize):
            self.items[i]=fixedexpress(nitems,itemsperset)
        
    def update(self,X,questions,allbest,allworst):
        self.rowiter=0
        [numofq,nitems]=X.shape
        draws=np.zeros((self.bsize,nitems))
        nb=np.bincount(allbest,minlength=nitems)
        nw=np.bincount(allworst,minlength=nitems)
        nq=np.bincount(questions.flatten(),minlength=nitems)
        w=(3*nb+nq-nw)/(2*nq+2*nb+2*nw)
        w[nq==0]=0
        var=w*(1-w)/nq
        var[nq==0]=100
        cov=np.diag(var)
        draws[:,:]=np.random.multivariate_normal(w, cov,self.bsize)
        if self.thres:
            order=np.argsort(draws)[:,::-1]
            threshold=(draws[range(self.bsize),order[:,self.k-1]]+draws[range(self.bsize),order[:,self.k]])/2
            draws=-np.abs(threshold-draws.T).T
        #w=estimateparamsbayes(X,questions,Xb,Xw,w0=[],weights=np.ones(numofq))
        if self.dif:
            drawsdif=np.zeros((self.bsize,nitems))
            for i in range(self.bsize):
                #drawsdif[i]=-nq/np.max(nq)+np.random.randn(nitems)*1
                drawsdif[i]=np.random.randn(nitems)*1
            
        for i in range(self.bsize):
            if self.dif:
                start=np.argsort(draws[i])[::-1][:self.nondif]
                difsort=np.argsort(drawsdif[i])[::-1]
                diffuse=[x for x in difsort if x not in start]
                self.items[i,:self.nondif]=start
                self.items[i,self.nondif:]=diffuse[:self.dif]
            else:
                start=np.argsort(draws[i])[::-1][:self.itemsperset]
                self.items[i,:]=start
    
    def get_questions(self,design):
        items=self.items[self.rowiter]
        self.rowiter+=1
        return items[design]

class winapproxthres:
    def __init__(self,nitems,k, bsize, itemsperset):
        self.wa=winapprox(nitems,k, bsize, itemsperset,thres=1)
        
    def update(self,X,questions,allbest,allworst):
        self.wa.update(X,questions,allbest,allworst)

    
    def get_questions(self,design):
        return self.wa.get_questions(design)

class winmismin:
    def __init__(self,nitems,k, bsize, itemsperset,N=100):
        self.bsize = bsize
        self.k=k
        self.itemsperset = itemsperset
        self.items=np.zeros((bsize,itemsperset),dtype=np.int)
        self.N=N
        self.rowiter=0
        for i in range(bsize):
            self.items[i]=fixedexpress(nitems,itemsperset)
        
    def update(self,X,questions,allbest,allworst):
        self.rowiter=0
        [numofq,nitems]=X.shape
        draws=np.zeros((self.N,nitems))
        nb=np.bincount(allbest,minlength=nitems)
        nw=np.bincount(allworst,minlength=nitems)
        nq=np.bincount(questions.flatten(),minlength=nitems)
        w=(3*nb+nq-nw)/(2*nq+2*nb+2*nw)
        w[nq==0]=0
        var=w*(1-w)/nq
        var[nq==0]=100
        cov=np.diag(var)
        draws[:,:]=np.random.multivariate_normal(w, cov,self.N)
        

        score=np.bincount(np.argsort(draws,1)[:,::-1][:,:self.k].flatten(),
                          minlength=nitems)/self.N
        order=np.argsort(w)[::-1]
        score[order[:self.k]]=1-score[order[:self.k]]
        var=.5/(np.bincount(questions.flatten(),minlength=nitems)+.01)
        zeros=np.zeros(nitems)
        
        for i in range(self.bsize):
            newscore=score+np.random.normal(zeros,np.sqrt(var))
            start=np.argsort(newscore)[::-1][:self.itemsperset]
            self.items[i]=start
    
    def get_questions(self,design):
        items=self.items[self.rowiter]
        self.rowiter+=1
        return items[design]

def run100trials(samplescheme,filename,k=10,itemnum=120,dnum=30,mis=0,n=100):
    if itemnum==300:
        itemfile='HB_300items.csv'
        nitems=300
    elif itemnum==40:
        itemfile='HB_40items.csv'
        nitems=40
    else:
        itemfile='HB_120items.csv'
        nitems=120
    if dnum==20:
        designfile='20itemMD_MD20_Design.csv'
        nofqsbot=12
    else:
        designfile='30itemMD_MD30_Design.csv'
        nofqsbot=18
    
    logging.basicConfig(filename='sim.log',level=logging.INFO)
    logger = logging.getLogger(__name__)

    numresp=500
    choicedata=np.zeros((n*numresp*nofqsbot,11),dtype=np.int)
    batchsize=20
    numrows=numresp//batchsize
    allbetas=np.zeros((numrows*n,nitems+2))
    logger.info(filename)
    for i in range(n):
        
        logger.info('iteration: %s', i)
            
        betas,allquestions,allbest,allworst,people=simulation(
            numresp,k,samplescheme,batchsize,itemfile,designfile,mis)
        rowstart=i*numresp*nofqsbot
        rowend=(i+1)*numresp*nofqsbot
        choicedata[rowstart:rowend,0]=i+1
        for j,person in enumerate(people):
            choicedata[rowstart+j*nofqsbot:rowstart+(j+1)*nofqsbot,1]=person
            choicedata[rowstart+j*nofqsbot:rowstart+(j+1)*nofqsbot,2]=j
            choicedata[rowstart+j*nofqsbot:rowstart+(j+1)*nofqsbot,3]=[x for x in range(1,nofqsbot+1)]
        choicedata[rowstart:rowend,4:9]=allquestions+1
        choicedata[rowstart:rowend,9]=allbest+1
        choicedata[rowstart:rowend,10]=allworst+1
        allbetas[i*numrows:(i+1)*numrows,0]=i+1
        allbetas[i*numrows:(i+1)*numrows,1]=[x for x in range(20,numrows*20+20,20)]
        allbetas[i*numrows:(i+1)*numrows,2:]=betas
    choicedata_df=pd.DataFrame(choicedata)
    choicedata_df.columns=["Iter","original_ids","RespIDNum","QNum",
                           "Choice1","Choice2","Choice3","Choice4","Choice5",
                           "Best","Worst"]
    allbetas_df=pd.DataFrame(allbetas)
    colname=["Iter","RespNum"]
    for i in range(nitems):
        colname+=["Item_"+str(i+1)]
    allbetas_df.columns=colname
    choicedata_df.to_csv("Results/choice_data-"+filename)
    allbetas_df.to_csv("Results/bhist-"+filename)


import sys

if __name__ == "__main__":# this decides which ones you run
    #filename="TSapprox-120v20.csv"
    #run100trials(TSapprox,filename,k=10,itemnum=120,dnum=20)
    #filename="TSapproxthes-120v20k10.csv"
    #run100trials(TSapproxthres,filename,k=10,itemnum=120,dnum=20)
    #filename="misminapprox-120v20k10.csv"
    #run100trials(misclassMinapprox,filename,k=10,itemnum=120,dnum=20)
    '''
    filename="winmismin-120v20k10.csv"
    run100trials(winmismin,filename,k=10,itemnum=120,dnum=20)

    filename="winmismin-120v20k20.csv"
    run100trials(winmismin,filename,k=20,itemnum=120,dnum=20)
    '''
    '''
    filename="newTSregthres-120v20k10.csv"
    run100trials(TSregthres,filename,k=10,itemnum=120,dnum=20,mis=0)
    '''
    '''
    
    filename="TSe2d4-120v20.csv"
    run100trials(TS24,filename,k=3,itemnum=120,dnum=20)
    filename="TSe4d2-120v20.csv"
    run100trials(TS42,filename,k=3,itemnum=120,dnum=20)
    filename="TSe2d2-120v20.csv"
    run100trials(TS22,filename,k=3,itemnum=120,dnum=20)
    filename="TSe2d0-120v20.csv"
    run100trials(TS20,filename,k=3,itemnum=120,dnum=20)
    filename="TSe4d0-120v20.csv"
    run100trials(TS40,filename,k=3,itemnum=120,dnum=20)
    filename="TSe1d2-120v20.csv"
    run100trials(TS12,filename,k=3,itemnum=120,dnum=20)
    filename="TSe1d4-120v20.csv"
    run100trials(TS14,filename,k=3,itemnum=120,dnum=20)
    '''
    '''
    filename="winapprox-120v20.csv"
    run100trials(winapprox,filename,k=20,itemnum=120,dnum=20)
    
    filename="winapproxthres-120v20k10.csv"
    run100trials(winapproxthres,filename,k=10,itemnum=120,dnum=20)
    
    filename="winapproxthres-120v20k20.csv"
    run100trials(winapproxthres,filename,k=20,itemnum=120,dnum=20)
    
    filename="winapproxthres-120v20k40.csv"
    run100trials(winapproxthres,filename,k=40,itemnum=120,dnum=20)
    '''
 
    '''
    filename="winapprox-120v30.csv"
    run100trials(winapprox,filename,k=3,itemnum=120,dnum=30)
    '''
    '''
    filename="mismin-120v20k10c5.csv"
    run100trials(misclassMin,filename,k=10,itemnum=120,dnum=20)
    filename="uncert-120v20k10c5.csv"
    run100trials(greatUncert,filename,k=10,itemnum=120,dnum=20)
    '''
    filename="fixed_express-120v20-moremis.csv"
    run100trials(FixedExpressClass,filename,k=3,itemnum=120,dnum=20,mis=3)
    '''
    filename="TSe4-120v20-moremis.csv"
    run100trials(TS,filename,k=3,itemnum=120,dnum=20,mis=3)

    filename="TS-120v20-moremis.csv"
    run100trials(TSreg,filename,k=3,itemnum=120,dnum=20,mis=3)
    filename="greedy-120v20-moremis.csv"
    run100trials(greedy,filename,k=3,itemnum=120,dnum=20,mis=3)
    filename="TSthres-120v20k3-moremis.csv"
    run100trials(TSthres,filename,k=3,itemnum=120,dnum=20,mis=3)
    filename="greedythres-120v20k3-moremis.csv"
    run100trials(greedythres,filename,k=3,itemnum=120,dnum=20,mis=3)
    filename="TSregthres-120v20k3-moremis.csv"
    run100trials(TSregthres,filename,k=3,itemnum=120,dnum=20,mis=3)
    filename="mismin-120v20k3-moremis.csv"
    run100trials(misclassMin,filename,k=3,itemnum=120,dnum=20,mis=3)
    filename="uncert-120v20k3-moremis.csv"
    run100trials(greatUncert,filename,k=3,itemnum=120,dnum=20,mis=3)
    filename="winapprox-120v3-moremis.csv"
    run100trials(winapprox,filename,k=3,itemnum=120,dnum=20,mis=3)
    '''
    '''
    filename="TSe4-120v20-mis.csv"
    run100trials(TS,filename,k=3,itemnum=120,dnum=20,mis=3)

    filename="TS-120v20-mis.csv"
    run100trials(TSreg,filename,k=3,itemnum=120,dnum=20,mis=3)
    filename="greedy-120v20-mis.csv"
    run100trials(greedy,filename,k=3,itemnum=120,dnum=20,mis=3)
    filename="TSthres-120v20k10-mis.csv"
    run100trials(TSthres,filename,k=3,itemnum=120,dnum=20,mis=3)
    filename="greedythres-120v20k10-mis.csv"
    run100trials(greedythres,filename,k=3,itemnum=120,dnum=20,mis=3)
    filename="TSregthres-120v20k10-mis.csv"
    run100trials(TSregthres,filename,k=3,itemnum=120,dnum=20,mis=3)
    filename="mismin-120v20k10-mis.csv"
    run100trials(misclassMin,filename,k=3,itemnum=120,dnum=20,mis=3)
    filename="uncert-120v20k10-mis.csv"
    run100trials(greatUncert,filename,k=3,itemnum=120,dnum=20,mis=3)
    filename="winapprox-120v20-moremis.csv"
    run100trials(winapprox,filename,k=3,itemnum=120,dnum=20,mis=3)
    '''
    '''
    filename="TSe4-120v20.csv"
    run100trials(TS,filename,k=10,itemnum=120,dnum=20)

    filename="TSthres-120v20k10.csv"
    run100trials(TSthres,filename,k=10,itemnum=120,dnum=20)


    filename="TSthres-120v20k20.csv"
    run100trials(TSthres,filename,k=20,itemnum=120,dnum=20)


    filename="TSthres-120v20k40.csv"
    run100trials(TSthres,filename,k=40,itemnum=120,dnum=20)
    
    filename="TSthres-120v20k3.csv"
    run100trials(TSthres,filename,k=3,itemnum=120,dnum=20)
    
    
    filename="TSe4-40v20.csv"
    run100trials(TS,filename,k=10,itemnum=40,dnum=20)

    filename="TS-40v20.csv"
    run100trials(TSreg,filename,k=10,itemnum=40,dnum=20)
    filename="greedy-40v20.csv"
    run100trials(greedy,filename,k=10,itemnum=40,dnum=20)
    
    filename="TSthres-40v20k3.csv"
    run100trials(TSthres,filename,k=3,itemnum=40,dnum=20)
    filename="greedythres-40v20k3.csv"
    run100trials(greedythres,filename,k=3,itemnum=40,dnum=20)
    filename="TSregthres-40v20k3.csv"
    run100trials(TSregthres,filename,k=3,itemnum=40,dnum=20)
    filename="mismin-40v20k3.csv"
    run100trials(misclassMin,filename,k=3,itemnum=40,dnum=20)
    filename="uncert-40v20k3.csv"
    run100trials(greatUncert,filename,k=3,itemnum=40,dnum=20)
    
    filename="TSthres-40v20k10.csv"
    run100trials(TSthres,filename,k=10,itemnum=40,dnum=20)
    filename="greedythres-40v20k10.csv"
    run100trials(greedythres,filename,k=10,itemnum=40,dnum=20)
    filename="TSregthres-40v20k10.csv"
    run100trials(TSregthres,filename,k=10,itemnum=40,dnum=20)
    filename="mismin-40v20k10.csv"
    run100trials(misclassMin,filename,k=10,itemnum=40,dnum=20)
    filename="uncert-40v20k10.csv"
    run100trials(greatUncert,filename,k=10,itemnum=40,dnum=20)
    

    filename="fixed_express-40v20.csv"
    run100trials(FixedExpressClass,filename,k=10,itemnum=40,dnum=20)
    '''
    '''
    filename="TSe4-120v30-mis.csv"
    run100trials(TS,filename,k=3,itemnum=120,dnum=30,mis=3)
    filename="TS-120v30-mis.csv"
    run100trials(TSreg,filename,k=3,itemnum=120,dnum=30,mis=3)
    filename="greedy-120v30-mis.csv"
    run100trials(greedy,filename,k=3,itemnum=120,dnum=30,mis=3)
    filename="newTSthres-120v30k3-mis.csv"
    run100trials(TSthres,filename,k=3,itemnum=120,dnum=30,mis=3)
    filename="greedythres-120v30k3-mis.csv"
    run100trials(greedythres,filename,k=3,itemnum=120,dnum=30,mis=3)
    filename="newTSregthres-120v30k3-mis.csv"
    run100trials(TSregthres,filename,k=3,itemnum=120,dnum=30,mis=3)
    filename="mismin-120v30k3-mis.csv"
    run100trials(misclassMin,filename,k=3,itemnum=120,dnum=30,mis=3)
    filename="uncert-120v30k3-mis.csv"
    run100trials(greatUncert,filename,k=3,itemnum=120,dnum=30,mis=3)
    '''
    #filename="fixed_express-120v20-mistest.csv"
    #run100trials(FixedExpressClass,filename,k=3,itemnum=120,dnum=20,mis=3)
    #filename="TS-120v20test.csv"
    #run100trials(TSreg,filename,k=3,itemnum=120,dnum=20)
    #filename="greedynightmare-120v20.csv"
    #run100trials(greedynightmare,filename,k=3,itemnum=120,dnum=20)

    #filename="TSe2-120v20.csv"
    #run100trials(TSmore,filename,k=3,itemnum=120,dnum=20)

    #filename="TSe1-120v20.csv"
    #run100trials(TSall,filename,k=3,itemnum=120,dnum=20)
    '''
    filename="TSe3-120v30-mis.csv"
    run100trials(TS,filename,k=3,itemnum=120,dnum=30,mis=3)
    filename="TSe3apprx-120v30-mis.csv"
    run100trials(TSapprox,filename,k=3,itemnum=120,dnum=30,mis=3)
    filename="greedy3-120v30-mis.csv"
    run100trials(greedy,filename,k=3,itemnum=120,dnum=30,mis=3)
    '''
    '''
    filename="fixed_express-300v20.csv"
    run100trials(FixedExpressClass,filename,k=10,itemnum=300,dnum=20)
    filename="TSe4-300v20.csv"
    run100trials(TS,filename,k=10,itemnum=300,dnum=20)

    filename="TS-300v20.csv"
    run100trials(TSreg,filename,k=10,itemnum=300,dnum=20)
    filename="greedy-300v20.csv"
    run100trials(greedy,filename,k=10,itemnum=300,dnum=20)
    
    filename="newTSthres-300v20k3.csv"
    run100trials(TSthres,filename,k=3,itemnum=300,dnum=20)
    filename="greedythres-300v20k3.csv"
    run100trials(greedythres,filename,k=3,itemnum=300,dnum=20)
    filename="newTSregthres-300v20k3.csv"
    run100trials(TSregthres,filename,k=3,itemnum=300,dnum=20)
    filename="mismin-300v20k3.csv"
    run100trials(misclassMin,filename,k=3,itemnum=300,dnum=20)
    filename="uncert-300v20k3.csv"
    run100trials(greatUncert,filename,k=3,itemnum=300,dnum=20)
    
    filename="newTSthres-300v20k10.csv"
    run100trials(TSthres,filename,k=10,itemnum=300,dnum=20)
    filename="greedythres-300v20k10.csv"
    run100trials(greedythres,filename,k=10,itemnum=300,dnum=20)
    filename="newTSregthres-300v20k10.csv"
    run100trials(TSregthres,filename,k=10,itemnum=300,dnum=20)
    filename="mismin-300v20k10.csv"
    run100trials(misclassMin,filename,k=10,itemnum=300,dnum=20)
    filename="uncert-300v20k10.csv"
    run100trials(greatUncert,filename,k=10,itemnum=300,dnum=20)
    
    filename="newTSthres-300v20k20.csv"
    run100trials(TSthres,filename,k=20,itemnum=300,dnum=20)
    filename="greedythres-300v20k20.csv"
    run100trials(greedythres,filename,k=20,itemnum=300,dnum=20)
    filename="newTSregthres-300v20k20.csv"
    run100trials(TSregthres,filename,k=20,itemnum=300,dnum=20)
    filename="mismin-300v20k20.csv"
    run100trials(misclassMin,filename,k=20,itemnum=300,dnum=20)
    filename="uncert-300v20k20.csv"
    run100trials(greatUncert,filename,k=20,itemnum=300,dnum=20)
    
    filename="newTSthres-300v20k40.csv"
    run100trials(TSthres,filename,k=40,itemnum=300,dnum=20)
    filename="greedythres-300v20k40.csv"
    run100trials(greedythres,filename,k=40,itemnum=300,dnum=20)
    filename="newTSregthres-300v20k40.csv"
    run100trials(TSregthres,filename,k=40,itemnum=300,dnum=20)
    filename="mismin-300v20k40.csv"
    run100trials(misclassMin,filename,k=40,itemnum=300,dnum=20)
    filename="uncert-300v20k40.csv"
    run100trials(greatUncert,filename,k=40,itemnum=300,dnum=20)
    '''

