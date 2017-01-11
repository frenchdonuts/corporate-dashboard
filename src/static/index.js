var Chance = require('chance');
// pull in CSS
require('./styles/main.scss');

// Fullscreen our Elm App
var Elm = require('../elm/Main');
var app = Elm.Main.fullscreen();

window.setInterval(createAndSendIssue, 3 * 1000);

function createAndSendIssue() {
    //
    var submissionTimestamp = new Date().getTime();
    var customerName = chance.name();
    var customerEmailAddress = chance.email();
    var description = chance.sentence();
    var open = false;
    var closedTimestamp = -1;
    var employeeName = chance.name();

    var issue = {
            submissionTimestamp: submissionTimestamp,
            customerName: customerName,
            customerEmailAddress: customerEmailAddress,
            description: description,
            open: open,
            closedTimestamp: closedTimestamp,
            employeeName: employeeName
        }
        //console.log(issue)
    app.ports.issues.send(issue);

}
