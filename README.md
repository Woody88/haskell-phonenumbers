haskell-phonenumbers
============================

Haskell Phonenumber Library using Google libphonenumber (python version) and [John Millikin](https://john-millikin.com/)'s [`cpython`](http://hackage.haskell.org/package/cpython) package
written by John Millikin.

Important Notes
---------------

Please have Stack installed. It will install GHC and other important libraries.
There is also a link to installing haskell platform if you wish to have it on their machine.
* [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
* [Haskell](https://www.haskell.org/downloads)

Also your machine must have
python verison 3.1+ installed.

For Debian/Ubuntu this python3.x-dev will be required:
    ```bash
    apt-get install python3.1-dev
    ```

Setup
1. Clone this repository:

    ```bash
    git clone https://github.com/Woody88/haskell-phonenumbers.git
    ```
2. Copy Google libphonenumber python library `python-phonenumbers/python/phonenumbers` into python main   
   directory.

3. A useful way to find your python lib path is by running the command below.
   On linux machine it will mostly be `usr/lib/python3.x/`:
  ```bash
   mymachine@mymachine:~$ python
   Python 3.4.7 (default, Aug 29 2017, 03:45:21)
   [GCC 5.4.0 20160609] on linux
   Type "help", "copyright", "credits" or "license" for more information.
   >>> import  os
   <module 'posixpath' from '/usr/lib/python3.4/posixpath.py'>
   ```
4. Run `stack setup` project root folder.

5. Run `stack build` project root folder.

6. Start server with `stack exec haskell-phonenumbers-exe`

Usage
-----
Accepted phone number format +14165555555 or +1-613-321-1114 Canadian or American.
+Country Number-Region-phonenumber

Get a parsed phone number `GET /api/phonenumbers/parse/text/+14165555555`:
```bash
 curl -X GET -H  "Accept: application/json" http://localhost:8080/api/phonenumbers/parse/text/+14165555555
 ["+1 416-555-5555"]
```

Post a file containing phonenumber with above format encoded in base64:
```bash
curl -X POST -H "Content-Type: text/plain;charset=UTF-8" -H "Accept: application/json" -d "MTUxMDc0ODgyMzAgMTcwMzQ4MDA1MDAg" http://localhost:8080/api/phonenumbers/parse/text
["+1 510-748-8230","+1 703-480-0500"]
```
