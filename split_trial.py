#!/usr/bin/env python
# coding: utf-8

# In[12]:


import pandas as pd


# In[13]:



import os

print("Current Working Directory " , os.getcwd())


# In[14]:


os.chdir("E:/data")


# In[15]:


chunk_size = 1000000
batch_no = 1


# In[16]:


for chunk in pd.read_csv('news.csv', chunksize = chunk_size):
    chunk.to_csv('news' + str(batch_no) + '.csv', index=False)
    batch_no +=1


# In[ ]:





# In[ ]:




