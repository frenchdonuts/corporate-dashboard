var fs = require('fs');
var Chance = require('chance');
var capitals = require('./capitals')


main();

function main() {
    var chance = new Chance();

    // Generate data for our Geospatial View
    generateGeospatialJSON(chance);

    var openIssuesCount = 30;

    // Generate data for our Key Metrics View
    generateKeyMetricJSON(chance, openIssuesCount);

    // Generate data for our Data View
    generateCSV(chance, 'issues', openIssuesCount);
}

function generateGeospatialJSON(chance) {
    var writeStream = fs.createWriteStream('./src/static/geospatial.json');

    var json = JSON.stringify(generateGeospatialObject(chance));
    writeStream.write(json);
    writeStream.end();
}

function generateGeospatialObject(chance) {
    var object = {};

    for (var i = 0; i < capitals.length; i++) {
        var capital = capitals[i];
        object[capital.capital_name] = {
            latitude: capital.latitude,
            longitude: capital.longitude,
            employeeCount: chance.integer({
                min: 1000,
                max: 10000
            })
        }
    }

    return object;
}

function generateKeyMetricJSON(chance, openIssuesCount) {
    //
    var writeStream = fs.createWriteStream('./src/static/key_metrics.json');

    var json = JSON.stringify(generateKeyMetricsObject(chance, openIssuesCount));
    writeStream.write(json);
    writeStream.end();
}

function generateKeyMetricsObject(chance, openIssuesCount_) {
    //
    var payingCustomersOverTimeData_ = [];
    for (var i = 0; i < 12; i++) {
        payingCustomersOverTimeData_.push(chance.integer({
            min: 2000,
            max: 20000
        }));
    }

    var issuesOverTimeData_ = [];
    for (var i = 0; i < 11; i++) {
        issuesOverTimeData_.push(chance.integer({
            min: 2000,
            max: 5000
        }));
    }
    issuesOverTimeData_.push(openIssuesCount_)

    return {
        openIssuesCount: openIssuesCount_,
        payingCustomersOverTimeData: payingCustomersOverTimeData_,
        issuesOverTimeData: issuesOverTimeData_
    };
}

function generateCSV(chance, fileName, n) {
    var writeStream = fs.createWriteStream('./src/static/' + fileName + '.csv');

    var i = 0;
    while (i < n) {
        writeStream.write(generateRow(chance) + "\n");
        i++;
    }

    writeStream.end();
}

function generateRow(chance) {
    var submissionTimestamp = chance.timestamp() * 1000;
    var customerName = chance.name();
    var customerEmailAddress = chance.email();
    var description = chance.sentence();
    var openClosedStatus = chance.bool();

    var timestampMod = Date.now() - submissionTimestamp;
    var closedTimestamp = (chance.timestamp() * 1000) % timestampMod +
        submissionTimestamp;

    var employeeName = chance.name();

    var row = [submissionTimestamp, customerName, customerEmailAddress,
        description, openClosedStatus, closedTimestamp, employeeName
    ];

    return row.toString();
}
