package com.ditcalendar.bot

import com.ditcalendar.bot.config.*
import com.elbekD.bot.Bot
import com.elbekD.bot.server
import com.github.kittinunf.result.Result

fun main(args: Array<String>) {

    val config by config()

    val token = config[telegram_token]
    val herokuApp = config[heroku_app_name]

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

    bot.onCommand("/start") { msg, _ ->
        val calendarCommand = DitCalendarCommand()

        val calendarId: Long = 1

        when (val result = calendarCommand.getCalendarAndTask(calendarId)) {
            is Result.Success ->
                bot.sendMessage(msg.chat.id, result.value, "MarkdownV2")
            is Result.Failure -> {
                result.error.printStackTrace()
                bot.sendMessage(msg.chat.id, "kein Kalendar")
            }
        }
    }

    bot.onCommand("/assaignToTask") { msg, opts ->
        val calendarCommand = DitCalendarCommand()

        val taskId: Long = 1

        msg.from?.also {
            when (val result = calendarCommand.assignUserToTask(taskId, it)) {
                is Result.Success ->
                    bot.sendMessage(msg.chat.id, "Ok")
                is Result.Failure -> {
                    result.error.printStackTrace()
                    bot.sendMessage(msg.chat.id, "Fehler")
                }
            }
        }
    }

    bot.onCommand("/echo") { msg, opts ->
        //bot.sendMessage(msg.chat.id, "${msg.text} ${opts ?: ""}")
        bot.sendMessage(msg.chat.id, "echo ${opts ?: ""}")
    }

    bot.start()
}