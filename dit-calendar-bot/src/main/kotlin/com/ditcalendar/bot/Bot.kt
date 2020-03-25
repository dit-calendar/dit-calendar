package com.ditcalendar.bot

import com.ditcalendar.bot.config.*
import com.elbekD.bot.Bot
import com.elbekD.bot.server

fun main(args: Array<String>) {

    val config by config()

    val token = config[telegram_token]
    val herokuApp = config[heroku_app_name]
    val calendarCommand = DitCalendarCommand()

    val bot = if (config[webhook_is_enabled]) {
        Bot.createWebhook(config[bot_name], token) {
            url = "https://$herokuApp.herokuapp.com/$token"

            /*
            Jetty server is used to listen to incoming request from Telegram servers.
            */
            server {
                host = "0.0.0.0"
                port = config[server_port]
            }
        }
    } else Bot.createPolling(config[bot_name], token)

    bot.onCommand("/start") { msg, opts ->
        val result = calendarCommand.parseRequest(msg, opts)
        bot.sendMessage(msg.chat.id, result, "MarkdownV2", true)
    }

    bot.start()
}