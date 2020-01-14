package com.ditcalendar.bot

import com.elbekD.bot.Bot
import com.elbekD.bot.server

fun main(args: Array<String>) {

    val config by config()

    val token = config[telegram_token]
    val herokuApp = config[heroku_app_name]

    val bot = Bot.createWebhook(config[bot_name], token) {
        url = "https://$herokuApp.herokuapp.com/$token"

        /*
            Jetty server is used to listen to incoming request from Telegram servers.
         */
        server {
            host = "0.0.0.0"
            port = config[server_port]
        }
    }

    bot.onCommand("/start") { msg, _ ->
        bot.sendMessage(msg.chat.id, "Hello World!")
    }

    bot.onCommand("/echo") { msg, opts ->
        //bot.sendMessage(msg.chat.id, "${msg.text} ${opts ?: ""}")
        bot.sendMessage(msg.chat.id, "echo ${opts ?: ""}")
    }

    bot.start()
}