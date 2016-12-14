var fs = require('fs');
var faker = require('faker');
var capitals = require('./capitals')


main();

function main() {
    // Generate data for our Geospatial View
    generateGeospatialJSON();

    var openIssuesCount = Math.floor(faker.random.number());

    // Generate data for our Key Metrics View
    generateKeyMetricJSON(openIssuesCount);

    // Generate data for our Data View
    generateCSV('issues', openIssuesCount);
}

function generateGeospatialJSON() {
    var writeStream = fs.createWriteStream('./geospatial.json');

    var json = JSON.stringify(generateGeospatialObject());
    writeStream.write(json);
    writeStream.end();
}

function generateGeospatialObject() {
    var object = {};

    console.log(capitals)
    for (var i = 0; i < capitals.length; i++) {
        var capital = capitals[i];
        object[capital.capital_name] = {
            latitude: capital.latitude,
            longitude: capital.longitude,
            employeeCount: Math.floor(faker.random.number())
        }
    }

    return object;
}

function generateKeyMetricJSON(openIssuesCount) {
    //
    var writeStream = fs.createWriteStream('./key_metrics.json');

    var json = JSON.stringify(generateKeyMetricsObject(openIssuesCount));
    writeStream.write(json);
    writeStream.end();
}

function generateKeyMetricsObject(openIssuesCount_) {
    //
    var payingCustomersOverTimeData_ = [];
    for (var i = 0; i < 12; i++) {
        payingCustomersOverTimeData_.push(Math.floor(faker.random.number()));
    }

    var issuesOverTimeData_ = [];
    for (var i = 0; i < 11; i++) {
        issuesOverTimeData_.push(Math.floor(faker.random.number()));
    }
    issuesOverTimeData_.push(openIssuesCount_)

    return {
        openIssuesCount: openIssuesCount_,
        payingCustomersOverTimeData: payingCustomersOverTimeData_,
        issuesOverTimeData: issuesOverTimeData_
    };
}

function generateCSV(fileName, n) {
    var writeStream = fs.createWriteStream('./' + fileName + '.csv');

    var i = 0;
    while (i < n) {
        writeStream.write(generateRow() + "\n");
        i++;
    }

    writeStream.end();
}

function generateRow() {
    var submissionTimestamp = faker.date.past();
    var customerName = faker.name.findName();
    var customerEmailAddress = faker.internet.email();
    var description = faker.lorem.sentence();
    var openClosedStatus = faker.random.boolean();
    var closedTimestamp = faker.date.recent();
    var employeeName = faker.name.findName();

    var row = [submissionTimestamp, customerName, customerEmailAddress,
        description, openClosedStatus, closedTimestamp, employeeName
    ];

    return row.toString();
}
