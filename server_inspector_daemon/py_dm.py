#!/usr/bin/python3.5
#python_daemon
#conan.py
import subprocess as sb
import json as j
import http.client
import datetime as dt
import sys
from os.path import isfile
from time import sleep, time


'''
whoJson==
    {
        "data":
        "users":
    }

PSJson== 
    {
        "tstmp: string or None, 
        "data":[obj, obj, obj, obj,...], 
        "users":[user1,user2,user3,...not duplicatively]
    }
mergeJson== 
    [
        {
            "tstmp": tstmp,
            "data":[obj, obj, obj, obj,...]
        },
        {
            "tstmp": tstmp,
            "data":[obj, obj, obj, obj,...]
        },
        {
            "tstmp": tstmp,
            "data":[obj, obj, obj, obj,...]
        },
    ]

'''

#leaves PSJson.json file, return None
def invokePS():
    def PS_parse(file, tstmp):
        lines_list=open(file).read().splitlines()#list of lines w/o trailing \n    
        dump_list=[]
        with open("PSJson.json", "w") as tj:
            for i in range(8,len(lines_list)-1): 
                split=lines_list[i].split()
                obj={}
                obj["user"]=split[0]            #user=col 1
                obj["cpu"]=float(split[1])      #cpu =col 8
                obj["proc"]=split[2]    #pname=col 11
                dump_list.append(obj)
            #tstmp dict
            tstmp_dict={}
            tstmp_dict["yr"]=int(tstmp[:4])
            tstmp_dict["month"]=int(tstmp[5:7])
            tstmp_dict["date"]=int(tstmp[8:10])
            tstmp_dict["second"]=int(tstmp[11:13])*3600+int(tstmp[14:16])*60+int(tstmp[-2:])
            wrap_dict={"tstmp":tstmp_dict, "data":dump_list}
            j.dump(wrap_dict,tj,indent =4)
        return None

    sb.run("ps axo user:20,pcpu,comm > PSout", shell=True)
    tstmp=str(dt.datetime.now()).split(".")[0] # yr-mon-dat hr:min:sec
    PS_parse("PSout",tstmp)
    return None

#leaves whoJson.json file, wuser return None
def invokeWho():
    def who_parse(file):
        lines_list=open(file).read().splitlines()
        dump_list= []
        with open("whoJson.json", "w") as wj:
            for line in lines_list:
                obj={}
                obj["user"]=line.split()[0]
                obj["cpu"]=0            #defaults to None
                obj["proc"]=None   
                dump_list.append(obj)
            j.dump(dump_list,wj,indent=4)
        return None      

    sb.run("who > whoout", shell=True)
    who_parse("whoout")
    return None


'''
def merge_Json(PSJson, whoJson): #both param are string
    #mergeJson initialized
    mergeJson="mergeJson.json"
    if not isfile(mergeJson):
        init = open(mergeJson, "w")
        init.close()
        mData=[]
    else:
        with open(mergeJson) as mj: 
            mData=j.load(mj)
    #open PSJson and whoJson 
    tj, wj= open(PSJson), open(whoJson) 
    tData, wData= j.load(tj), j.load(wj) 
    #1st: remove sysuser from PSJson loaded
    deathnote=[]
    for i,obj in enumerate(tData["data"]):
        if obj["user"] not in wData["users"]:
            #del tData["data"][i]
            deathnote.append(obj)
    #2nd: append idle whoJson users
    for i,user in enumerate(wData["users"]):
        if user not in tData["users"]:
            tData["users"].append(user)
            tData["data"].append(wData["data"][i])
    wj.close()
    tj.close()

    #3rd: merge it to mergeJson
    tj_w, mj_w = open(PSJson,"w"), open(mergeJson,"w") 
    for i in range(len(deathnote)):
        tData["data"].remove(deathnote[i])
    mData.append(tData) 
    del mData[-1]["users"] #now mData==[{"tstmp":tstmp, "data":[obj,obj,...]}] 
    j.dump(tData, tj_w)
    j.dump(mData, mj_w)
    tj_w.close()
    mj_w.close()


def checkHeavyUser(mergeJson): #Json==filename(str)==mergeJson.json
    #to the top three heavy users, send msg
    with open(mergeJson) as mj:
        mData=j.load(mj)       
        for i in range(3):
#            rank=i
            user=mData[-1]["data"][i]["user"]
            cpu =mData[-1]["data"][i]["cpu"]
            proc=mData[-1]["data"][i]["proc"]
            if cpu>70: 
                with open("msg.txt", "w") as msg:
                    msg.flush()
                    msg.write("Warning to %s: your process %s using %s of the cpu resource \n"%(user,proc,str(cpu)+"%")) 
                sb.run("cat msg.txt | write %s"%user, shell =True)
                print("\ngave %s cpu usage warning\n"%user)
'''

def merge_Json(PSJson, whoJson): #both param are string
    #open PSJson and whoJson
    tj, wj= open(PSJson), open(whoJson) 
    tData, wData= j.load(tj), j.load(wj) 
    wj.close()
    tj.close()

    who_users = []
    for obj in wData:
    	who_users.append(obj["user"])

    #1st: remove sysuser from PSJson loaded
    valid_dat=[]
    valid_user=[]
    for i,obj in enumerate(tData["data"]):
        if obj["user"] in who_users:
            valid_dat.append(obj)
            valid_user.append(obj["user"])

    #2nd: append idle whoJson users
    for i,obj in enumerate(wData):
        if obj["user"] not in valid_user:
            valid_dat.append(obj)
            valid_user.append(obj["user"])

    #mergeJson initialized
    mergeJson="mergeJson.json"
    if not isfile(mergeJson):
        mData=[]
    else:
        with open(mergeJson) as mj: 
            mData=j.load(mj)

    tData["data"] = valid_dat
    #3rd: merge it to mergeJson
    mData.append(tData)
    with open(mergeJson,"w") as mj_w:
    	j.dump(mData, mj_w, indent=4)


def checkHeavyUser(mergeJson): #Json==filename(str)==mergeJson.json
    #to the top three heavy users, send msg
    with open(mergeJson) as mj:
        mData=j.load(mj)       
        for i in range(3):
#            rank=i
            user=mData[-1]["data"][i]["user"]
            cpu =mData[-1]["data"][i]["cpu"]
            proc=mData[-1]["data"][i]["proc"]
            if cpu>70: 
                with open("msg.txt", "w") as msg:
                    msg.flush()
                    msg.write("Warning to %s: your process %s using %s of the cpu resource \n"%(user,proc,str(cpu)+"%")) 
                sb.run("cat msg.txt | write %s"%user, shell =True)
                print("\ngave %s cpu usage warning\n"%user)


def sendJson(Json, addr): # send Json file to http server
    conn=http.client.HTTPConnection(str(addr), 8004)
    conn.request("POST", "/data", open(Json))
    response = conn.getresponse()
    stat_code=response.status
    stat_word=http.client.responses[stat_code]
    print("\n%s, %s\n"%(stat_code,stat_word))

    conn.close()
    return None 

#    HTTPResponse.status
#    http.client.responses[]



def examineSys():
    PSJson, whoJson, mergeJson = "PSJson.json", "whoJson.json", "mergeJson.json"
    invokeWho()
    invokePS()
    merge_Json(PSJson,whoJson)
    checkHeavyUser(mergeJson)


if __name__=="__main__":
    address=sys.argv[1]
    print(" _____       _            _   _              _____                                      ")
    print("|  __ \     | |          | | (_)            / ____|                                     ")
    print("| |  | | ___| |_ ___  ___| |_ ___   _____  | |     ___  _ __   __ _ _ __    _ __  _   _ ")
    print("| |  | |/ _ \ __/ _ \/ __| __| \ \ / / _ \ | |    / _ \| '_ \ / _` | '_ \  | '_ \| | | |")
    print("| |__| |  __/ ||  __/ (__| |_| |\ V /  __/ | |___| (_) | | | | (_| | | | |_| |_) | |_| |")
    print("|_____/ \___|\__\___|\___|\__|_| \_/ \___|  \_____\___/|_| |_|\__,_|_| |_(_) .__/ \__, |")
    print("                                                                           | |     __/ |")
    print("                                                                           |_|    |___/ ")
    print("========================================================================================")
    print("               \nconnecting to HTTP server on %s\n"%address                              )
    print("========================================================================================")
    sb.run("rm mergeJson.json",shell=True)
    start=time()
    print("Conan.py: Im here for inspect your resource usage\n")
    while 1: 
        time_elapsed=time()-start
        print("trial %s: now let\'s see what\'s going on the server? (every 3sec)"%(int(time_elapsed/3)))
        examineSys()
#        if int(time_elapsed/3600)>0:
        if int(time_elapsed/30)>0:
            print("\nreport the log to the HTTP server")            
            #print("running for %s hr(s)"%(int(time_elapsed/3600)))
            sendJson("mergeJson.json", address)
            start=time()
            sb.run("rm mergeJson.json",shell=True)
        sleep(3)
