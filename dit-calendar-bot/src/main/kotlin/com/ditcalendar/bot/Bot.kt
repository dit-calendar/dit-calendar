package com.ditcalendar.bot

import com.ditcalendar.bot.config.*
import com.elbekD.bot.Bot
import com.elbekD.bot.server

fun main(args: Array<String>) {

    val config by config()

    val token = config[telegram_token]
    val herokuApp = config[heroku_app_name]

    val calendarCommand = CalendarCommand()


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
        val calendarId: Long = 1
        val calendar = calendarCommand.getCalendarAndTask(calendarId)
        if(calendar != null)
            bot.sendMessage(msg.chat.id, calendar,"MarkdownV2")
        else
            bot.sendMessage(msg.chat.id, "kein Kalendar")
    }

    bot.onCommand("/echo") { msg, opts ->
        //bot.sendMessage(msg.chat.id, "${msg.text} ${opts ?: ""}")
        bot.sendMessage(msg.chat.id, "echo ${opts ?: ""}")
    }

    bot.start()
}