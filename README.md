Detective Conan.py: 17' Networking toy project
==========================================================    
Excessive Server User inspector via collecting logs and warning


Components
-----------------------   
## Inspected server side

### py_dm.py  (under server_inspector_daemon directory): Server inspector daemon   
- collects logs and reports to HTTP server with <code>.json</code> extension
- write msg to user directly on terminal with <code>write</code> command 

## HTTP server

### *hs
### css
- analyzes the data received   
- generate scoreboard with <code>.html</code>
- send email to top 1,2,3 excessive users. Assumed email address is collected at the time the user registers on server

Dependency & Test environment
----------------------------------------
### (inspected) serverside
##### Python 3.5.2 
##### Ubuntu 12.04

### HTTP server
##### Haskell (Glascow v8.0.2)
##### Ubuntu 16.04 


How to Use
--------------------------------
1) run HTTP server and figure out http server ip
2) run py_dm.py on the server you want to inspect as follows     
 <code> $ ./py_dm.py <http server ip>  </code>
3) that's it!
