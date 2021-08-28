### Env ###

 - Erlang: 22.3.4.1
 - Elastic Seach: 7.x

## Build Sos Backend ##

To compile sos Backend, go to the main repo directory $REPO and execute the command

	$ make

If compile fail with error can not build fcm. Run this command:


	$ cd _build/default/lib/fcm && make


To generate sos release 

	$make rel

## Add hostname ##

    sudo echo "127.0.0.1 backend-01.local" >> /etc/hosts

## Running Sos backend. ##

To run Sos Backend from the project tree after compiling it, change to $REPO/rel/sos.

There you can use the Sos command line script to start and stop Sos. For example:

    $ ./_build/default/rel/sos/bin/sos start

will start the server.

Test server run :

    $ ./_build/default/rel/sos/bin/sos ping 


Stop server :

     $ ./_build/default/rel/sos/bin/sos stop


Debug:

     ./_build/default/rel/sos/bin/sos remote_console

REST API TEST :

<code bash>

curl --location --request POST 'http://localhost:8080/api/v1/auth' \
--header 'Content-Type: application/json' \
--data-raw '{
  "username":"sos.demo@gmail.com",
  "password":"123456789", 
  "grant_type":"password", 
  "scope":"USER"
}'

</code>
