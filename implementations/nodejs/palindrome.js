#! /usr/local/bin/node

const readline = require('readline');
const fs = require('fs');
const MAXARGS = 3;
const INFILEINDEX = 1;
const MAXRECINDEX = 2;

let inStream = null;
let inFilename = 'STDIN';
let linesRead = 0;
let maxRecords = 0;

function reverseString(str) {
    return (str.split('').reverse().join(''));
}

function isPalindrome(str) {
    if (reverseString(str.toUpperCase()) === str.toUpperCase()) {
	return (true);
    }
    
    return (false);
}

function processArgs(args) {
    if (CmdArgs.length > MAXARGS) {
	console.log('ERROR: Too many arguments.');
	process.exit(1);
    }

    if (CmdArgs.length > (INFILEINDEX)) {
	inFilename = CmdArgs[INFILEINDEX];

	if (!fs.existsSync(inFilename)) {
	    console.log('ERROR: File not found: ' + inFilename);
	    process.exit(1);
	}
    }
    
    if (CmdArgs.length > MAXRECINDEX) {
	maxRecords = Number(CmdArgs[MAXRECINDEX]);

	if (isNaN(maxRecords)) {
	    console.log('ERROR: Invalid max record value.');
	    process.exit(1);
	}
    }
}

function processRecord(str) {
    firstname = str.substring(0, 24).trimRight();
    lastname = str.substring(25).trimRight();
    
    console.log((isPalindrome(firstname) ? 'YES' : ' NO') +
		' ' +
		firstname.padEnd(25, ' ') +
		' ' +
		(isPalindrome(lastname) ? 'YES' : ' NO') +
		' ' +
		lastname.padEnd(25, ' ')
	       );

    return;
}
    
// Make this like a normal argv (command + args, lose '/usr/bin/node' as the first argument).
let CmdArgs = process.argv.slice(1);

processArgs(CmdArgs);
if (inFilename == 'STDIN') {
    inStream = readline.createInterface({
	input: process.stdin
    });
} else {
    inStream = readline.createInterface({
	input: fs.createReadStream(inFilename)
    });
}

inStream
    .on('line', function(line) {
	let firstname = null;
	let lastname = null;

	if ((maxRecords > 0) && (linesRead >= maxRecords)) {
	    inStream.close();
	}

	processRecord(line);
	linesRead++;
    })
    .on('close', function(line) {
	//    console.log('Total lines : ' + linesRead);
	process.exit(0);
    })
    .on('error', function(line) {
	console.log('Input file error: ${e.message}');
	process.exit(0);
    });
