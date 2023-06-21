const http = require('http');
const ws = require('ws');
const XMLHttpRequest = require("xmlhttprequest").XMLHttpRequest;

let port = 3709;

if (process.argv[2]) {
  port = Number(process.argv[2]);
}
console.log('Running client on port:', port);


let request = http.get('http://0.0.0.0:' + port + '/jsaddle.js', (res) => {
  if (res.statusCode !== 200) {
    console.error(`Did not get an OK from the server. Code: ${res.statusCode}`);
    res.resume();
    return;
  }

  let data = '';

  res.on('data', (chunk) => {
    data += chunk;
  });

  res.on('close', () => {
    console.log("Connecting to JSaddle running on port", port);
    eval(data);
  });
});

request.on('error', (err) => {
  console.error(`Encountered an error trying to make a request: ${err.message}`);
});
