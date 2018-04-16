/*-----------------------------------------------------------------------------
A simple echo bot for the Microsoft Bot Framework. 
-----------------------------------------------------------------------------*/

var restify = require('restify');
var builder = require('botbuilder');
var botbuilder_azure = require("botbuilder-azure");

// Setup Restify Server
var server = restify.createServer();
server.listen(process.env.port || process.env.PORT || 3978, function () {
   console.log('%s listening to %s', server.name, server.url); 
});
  
// Create chat connector for communicating with the Bot Framework Service
var connector = new builder.ChatConnector({
    appId: process.env.MicrosoftAppId,
    appPassword: process.env.MicrosoftAppPassword,
    openIdMetadata: process.env.BotOpenIdMetadata 
});

// Listen for messages from users 
server.post('/api/messages', connector.listen());

/*----------------------------------------------------------------------------------------
* Bot Storage: This is a great spot to register the private state storage for your bot. 
* We provide adapters for Azure Table, CosmosDb, SQL Azure, or you can implement your own!
* For samples and documentation, see: https://github.com/Microsoft/BotBuilder-Azure
* ---------------------------------------------------------------------------------------- */

var tableName = 'botdata';
//var azureTableClient = new botbuilder_azure.AzureTableClient(tableName, process.env['AzureWebJobsStorage']);
//var tableStorage = new botbuilder_azure.AzureBotStorage({ gzipData: false }, azureTableClient);

// Create your bot with a function to receive messages from the user
var bot = new builder.UniversalBot(connector);
//bot.set('storage', tableStorage);
var hours = []
var times = []
var memories = []
var sleep_message = ''
var mom_message = ''
var eat_message = ''
bot.dialog('/', [
    
    function (session) {
        builder.Prompts.text(session, "Hi! Tell me about something you accomplished or something good that happened today."); 
    },
    function (session, results) {
        var r = results.response;
        r = replace("/I|i/", "you"); // doesn't work
        r = r.replace("/my|My/", "your");
        r = r.replace("/mine|Mine/", "yours");
        r = r.replace("/I'm|i'm/", "you're");
        memories.push(r);
        builder.Prompts.number(session, "How many hours did you sleep last night?");
    },
    function (session, results) {
        hours.push(results.response);
        builder.Prompts.choice(session, "Have you called your mom today?", ["Yes", "No"]);
    },
    function(session, results){
        times.push(results.response);

        if (results.response.entity == 'Yes')
        {
            mom_message = "Good for you. Keeping in touch with family and friends can help you manage stress. "
        }
        else {
            mom_message = "Your mom probably misses you... Give her a call :) "
        }
        builder.Prompts.number(session, mom_message + "How many meals have you eaten in the past day?");
    }, 
    function (session, results) {
        if(results.response < 3)
        {
            eat_message = "Try and set aside time to sit down and eat three meals a day. "
        }
        else if(results.response > 3) {
            eat_message = "Good job. Just remember not to stress eat. "
        } else {
            eat_message = "Eating three meals a day can keep you energized. Good job!"
        }

        var total = 0
        for(var j = 0; j< hours.length; j++){
            total = total + hours[j];
        }
        var average = total/hours.length

        if(average < 7) {
            sleep_message = "Try and get some more sleep! "
        } else if(average > 9) {
            sleep_message = "You might be sleeping too much! "
        } else {
            sleep_message = "Good job! "
        }
        session.send(eat_message);
        session.send("Your sleep insights: You sleep for an average of  " + Math.floor(average*100)/100 + 
                    " hours each night. " + sleep_message + "Healthy adults need 7-9 hours of sleep nightly to feel rested. ")
        if(memories.length > 1) {
            session.send("Throwback: do you remember the time " + memories[Math.floor(Math.random()*(memories.length-1))] + "?");
        }
    }
]);