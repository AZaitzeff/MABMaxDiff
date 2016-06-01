
# coding: utf-8

# In[1]:

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import os
import seaborn
import scipy.stats as stats


# In[2]:

def getthemean(filename,colver='iteration',whichcol=['hr_top10']):
    data=pd.read_csv(filename)
    num=int(np.max(data[colver]))
    meandata=data[data[colver]==1][whichcol].get_values()
    for i in range(2,num+1):
        meandata+=data[data[colver]==i][whichcol].get_values()
    meandata/=num
    return meandata


# In[3]:

def getone(filename,colver='iteration',whichcol=['hr_top10'],iterval=1):
    data=pd.read_csv(filename)
    return data[data[colver]==iterval][whichcol].get_values()


# In[4]:

def getfiles(start="bhist",items='120',mis=0):
    listoffiles=[]
    where='../RealData'
    for file in os.listdir(where):
        if file.startswith(start):
            if items in file:
                if mis==0:
                    if '50' not in file:
                        listoffiles.append(file)
                elif mis==1:
                    if '50' in file:
                        listoffiles.append(file)
                else:
                    listoffiles.append(file)
    return where,listoffiles


# In[5]:

def getmaxmin(filename,colver='iteration',whichcol=['hr_top10']):
    data=pd.read_csv(filename)
    num=int(np.max(data[colver]))
    maxmindata=data[data[colver]==1][whichcol].get_values()
    if maxmindata.ndim==2:
        n,m=np.shape(maxmindata)
        maxmindata=np.empty((n,m,num))
        for i in range(1,num+1):
            maxmindata[:,:,i-1]=data[data[colver]==i][whichcol].get_values()
        sortedmaxmin=np.sort(maxmindata,-1)
        return sortedmaxmin[:,:,4],np.mean(sortedmaxmin,-1),sortedmaxmin[:,:,-5]
    else:
        n=np.shape(maxmindata)[0]
        maxmindata=np.empty((n,num))
        for i in range(1,num+1):
            maxmindata[:,i-1]=data[data[colver]==i][whichcol].get_values()
        sortedmaxmin=np.sort(maxmindata,-1)
        return sortedmaxmin[:,4],np.mean(sortedmaxmin,-1),sortedmaxmin[:,-5]


# In[6]:

def createlabel(f):
    name=f.split()
    label=[]
    typeoflab=name[1][1:]
    label.append(typeoflab)
    if typeoflab!='fixed_sparse':
        label.append(' $\epsilon$=')
        label.append(str(int(name[14])/int(name[6]))[:5])
        label.append(' $\delta$=')
        label.append(name[15][3:])
    if name[-2]!='0':
        label.append(' misinf')
    return ''.join(label)


# In[7]:

def maporder(A):
    trueutil=pd.read_csv('../MABMaxDiff/HB_120items.csv').get_values().mean(0)[2:]
    order=list(np.argsort(trueutil)[::-1])
    newfun=np.vectorize(order.index)
    return newfun(A-1)


# In[8]:

def histplots(indices=np.arange(0,6),whichcol='hr_top3',items='120',mis=0,name='name.pdf'):
    where,listoffiles=getfiles(start="hrhist",items=items,mis=mis)
    for i,f in enumerate(listoffiles):
        if i in indices:
            meandata=getthemean(where+'/'+f,colver='iteration',whichcol=whichcol)
            label=createlabel(f)
            plt.plot(range(20,1040,20),meandata,label=label)
    plt.ylabel(whichcol)
    plt.xlabel('respondent')
    plt.legend(loc='lower right',fontsize='small')
    plt.title('Top '+whichcol[6:]+' Hit Rate Over Respondents')
    plt.xlim([0,1050])
    plt.savefig(name, dpi=100)
    plt.close()



# In[9]:

def gettherank(matrix):
    n,m=np.shape(matrix)
    ranking=np.zeros((n,m))
    for i in range(n):
        array=matrix[i]
        temp = array.argsort()[::-1]
        ranks = np.empty(len(array), int)
        ranks[temp] = np.arange(len(array))+1
        ranking[i]=ranks
    return ranking


# In[10]:

def stplots(indices=np.arange(0,6),rank=[1,60],items='120',mis=0,name='name.pdf',sq=1):
    where,listoffiles=getfiles(start="sehist",items=items,mis=mis)
    whichcol=whichcols(rank)
    for i,f in enumerate(listoffiles):
        if i in indices:
            for j,col in enumerate(whichcol):
                meandata=getthemean(where+'/'+f,colver='V1',whichcol=col)
                if sq==0:
                    meandata=1/meandata**2 
                label=createlabel(f)
                plt.plot(range(20,1040,20),meandata,label=label+', '+str(rank[j])+' rank')
    if sq==0:
        plt.ylabel('Precision')
        plt.legend(loc='upper left',fontsize='small')
    else:
        plt.ylabel('Squared Error')
        plt.legend(loc='upper right',fontsize='small')
    plt.xlabel('respondent')
    
    if sq==0:
        plt.title('Precision Over Respondents')
    else:
        plt.title('Squared Error Over Respondents')
    plt.xlim([0,1050])
    plt.savefig(name, dpi=100)
    plt.close()


# In[11]:

def rundots(index,where,listoffiles):
    data=pd.read_csv(where+'/'+listoffiles[index])
    choices=data.get_values()[:,1:]
    res=maporder(choices)
    return res,createlabel(listoffiles[index][55:])


# In[12]:

def trueOrder():
    trueutil=pd.read_csv('../MABMaxDiff/HB_120items.csv').get_values().mean(0)[2:]
    order=list(np.argsort(trueutil)[::-1])
    return order


# In[13]:

def whichcols(rank):
    order=trueOrder()
    cols=[]
    for i in rank:
        cols.append('V'+str(order[i-1]+2))
    return cols


# In[14]:

def barplots(indices=[0],size=(10,4),name='name.pdf'):
    listoffiles=[]
    where='../RealData'
    for file in os.listdir(where):
        if file.startswith("itembyresp"):
                    listoffiles.append(file)
    n=len(indices)
    for i,index in enumerate(indices):
        res,label=rundots(index,where,listoffiles)
        plt.subplot('1'+str(n)+str(i+1))
        plt.plot(res,'ks', markersize=1)
        plt.xlim([0,1020])
        plt.title(label)
    fig = plt.gcf()
    fig.set_size_inches(size[0], size[1])
    fig.savefig(name, dpi=100)
    plt.close()


# In[15]:

def rankplot(indices=np.arange(0,6),rank=[1,2,3],iterval=1,items='120',mis=0,name='name.pdf',size=(10,6)):
    where,listoffiles=getfiles(start="bhist",items=items,mis=mis)
    whichcol=[]
    for i in range(120):
        whichcol.append('V'+str(i+3))
    n=len(indices)
    order=trueOrder()
    for i,f in enumerate(listoffiles):
        if i in indices:
            num=indices.index(i)+1
            meandata=getone(where+'/'+f,colver='V1',whichcol=whichcol,iterval=iterval)
            therank=gettherank(meandata)
            plt.subplot('1'+str(n)+str(num))
            l,k=np.shape(therank)
            comeback=[]
            for j in range(k):
                if order.index(j)+1 in rank:
                    comeback.append(j)
                else:
                    plt.plot(range(20,1040,20),therank[:,j],color = '0.75')
            for j in comeback:     
                plt.plot(range(20,1040,20),therank[:,j])
            plt.ylim(121,0)
            plt.xlim([20,1020])
            plt.title(createlabel(f))
            if num==1:
                plt.ylabel('rank')
            plt.xlabel('respondent')
    fig = plt.gcf()
    fig.set_size_inches(size[0], size[1])
    fig.savefig(name, dpi=100)
    plt.close()


# In[16]:

def bhplots(indices=np.arange(0,6),rank=[1,60],items='120',mis=0,name='name.pdf'):
    where,listoffiles=getfiles(start="bhist",items=items,mis=mis)
    whichcol=whichcols(rank)
    for i,f in enumerate(listoffiles):
        if i in indices:
            for j,col in enumerate(whichcol):
                mindata,meandata,maxdata=getmaxmin(where+'/'+f,colver='V1',whichcol=col)
                label=createlabel(f)
                p=plt.plot(range(20,1040,20),meandata,label=label+', '+str(rank[j])+' rank')
                c=plt.getp(p[0],'color')
                plt.fill_between(range(20,1040,20), mindata, maxdata,facecolor=c, alpha=0.5)
    plt.ylabel('Beta Value')
    plt.legend(loc='upper right',fontsize='small')
    plt.xlabel('respondent')
    plt.title('Beta Values: Mean and 90% Interval')
    plt.xlim([20,1020])
    plt.savefig(name, dpi=100)
    plt.close()


# In[17]:

histplots(indices=[0,5],whichcol='hr_top3',items='120',mis=0,name='plots/3hitrate120show2.pdf')
histplots(indices=[0,5],whichcol='hr_top10',items='120',mis=0,name='plots/10hitrate120show2.pdf')
histplots(whichcol='hr_top3',items='120',mis=0,name='plots/3hitrate120all.pdf')
histplots(whichcol='hr_top10',items='120',mis=0,name='plots/10hitrate120all.pdf')
histplots(whichcol='hr_top3',items='120',mis=1,name='plots/3hitrate120allmis.pdf')
histplots(whichcol='hr_top10',items='120',mis=1,name='plots/10hitrate120all.pdf')

histplots(whichcol='hr_top3',items='40',mis=0,name='plots/3hitrate40all.pdf')
histplots(whichcol='hr_top10',items='40',mis=0,name='plots/10hitrate40all.pdf')
histplots(whichcol='hr_top3',items='40',mis=1,name='plots/3hitrate40allmis.pdf')
histplots(whichcol='hr_top10',items='40',mis=1,name='plots/10hitrate40allmis.pdf')

histplots(whichcol='hr_top3',items='300',mis=0,name='plots/3hitrate300all.pdf')
histplots(whichcol='hr_top10',items='300',mis=0,name='plots/10hitrate300all.pdf')
histplots(whichcol='hr_top3',items='300',mis=1,name='plots/3hitrate300allmis.pdf')
histplots(whichcol='hr_top10',items='300',mis=1,name='plots/10hitrate300allmis.pdf')


# In[86]:

stplots(indices=[0,5],rank=[1,60],items='120',mis=0,name='plots/squarederror.pdf',sq=1)
stplots(indices=[0,5],rank=[1,60],items='120',mis=0,name='plots/precision.pdf',sq=0)


# In[87]:

barplots([0,3],size=(10,4),name='plots/2dotplot.pdf')
barplots([0,3,1],size=(10,4),name='plots/3dotplot.pdf')


# In[88]:

bhplots(indices=[0,5],rank=[1,60],items='120',mis=0,name='plots/betavalconfidence.pdf')


# In[89]:

rankplot(indices=[0,5],rank=np.arange(1,11),iterval=9,items='120',mis=0,name='plots/rankingcomp2.pdf')
rankplot(indices=[0,5,3],rank=np.arange(1,11),iterval=9,items='120',mis=0,name='plots/rankingcomp3.pdf')
rankplot(indices=[0,5,3],rank=np.arange(1,11),iterval=9,items='120',mis=1,name='plots/rankingcomp3mis.pdf')


# In[77]:

def norplots(indices=np.arange(0,6),rank=[1,60],items='120',mis=0,name='name.pdf',iterval=1,ts=0,after=0,xlim=[-2,5],ylim=[0,8]):
    colors=['b','g','r','c']
    where,listoffiles1=getfiles(start="sehist",items=items,mis=mis)
    where,listoffiles2=getfiles(start="bhist",items=items,mis=mis)
    whichcol=whichcols(rank)
    n=len(rank)
    z=0
    for i,f in enumerate(listoffiles1):
        if i in indices:
            for j,col in enumerate(whichcol):
                sehist=getone(where+'/'+listoffiles1[i],colver='V1',whichcol=col,iterval=iterval)
                bhist=getone(where+'/'+listoffiles2[i],colver='V1',whichcol=col,iterval=iterval)
                x=np.linspace(-2,5,1000)
                label=createlabel(f)
                plt.fill_between(x,0, stats.norm.pdf(x,bhist[ts], sehist[ts]),
                                 label=label+', '+str(rank[j])+' rank',
                                 alpha=0.5,facecolor=colors[(z*n+j)%4])
                if after:
                    plt.fill_between(x,0, stats.norm.pdf(x,bhist[ts], sehist[ts]*10),
                                 label=label+', '+str(rank[j])+' rank times 10',
                                 alpha=0.5,facecolor=colors[(z*n+j)%4])
            z+=1
    if not after:
        plt.legend(loc='upper right',fontsize='small')
        plt.title('Distribution of items at the ' +str((ts+1)*20)+'th respondent')
    else:
        plt.title('Distribution of rank '+str(rank[0])+' item at the ' +str((ts+1)*20)+'th respondent')
    plt.xlim(xlim)
    plt.ylim(ylim)
    plt.savefig(name, dpi=100)
    plt.close()


# In[ ]:




# In[78]:

norplots([0,5],rank=[1,90],ts=5,iterval=5,name='plots/beliefdistrBegin.pdf')
norplots([0,5],rank=[1,90],ts=50,iterval=5,name='plots/beliefdistrEnd.pdf')


# In[79]:

norplots([3],rank=[30],ts=5,iterval=5,name='plots/espdeltaBegin.pdf',after=1,xlim=[-3,3],ylim=[0,5])
norplots([3],rank=[30],ts=50,iterval=5,name='plots/espdeltaEnd.pdf',after=1,xlim=[-3,3],ylim=[0,5])



