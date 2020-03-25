package com.ditcalendar.bot

import com.ditcalendar.bot.config.*
import com.elbekD.bot.Bot
import com.elbekD.bot.server
import com.github.kittinunf.fuel.core.FuelError
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

    bot.onCommand("/start") { msg, opts ->
        val calendarCommand = DitCalendarCommand()
        val msgText = msg.text

        val result = if (msgText != null) {
            if (opts == null || !opts.startsWith("assign")) {
                val calendarId: Long = 1
                calendarCommand.getCalendarAndTask(calendarId)
            } else {
                val msgUser = msg.from
                val taskId: Long? = opts.substringAfter("_").toLongOrNull()

                if (msgUser != null && taskId != null)
                    calendarCommand.assignUserToTask(taskId, msg.chat.id, msgUser)
                else
                    Result.error(RuntimeException("request invalid"))
            }
        } else {
            Result.error(RuntimeException("request invalid"))
        }


        when (result) {
            is Result.Success ->
                bot.sendMessage(msg.chat.id, result.value, "MarkdownV2")
            is Result.Failure -> {
                val error = result.error
                if (error is FuelError) {
                    val response = error.response
                    when (response.statusCode) {
                        403 -> bot.sendMessage(msg.chat.id, "Zugriff vom bot nicht erlaubt")
                        404 -> bot.sendMessage(msg.chat.id, "kein Kalendar gefunden")
                        else -> bot.sendMessage(msg.chat.id, "unbekannter Fehler")
                    }

                } else {
                    bot.sendMessage(msg.chat.id, "unbekannter Fehler")
                }
                result.error.printStackTrace()
            }
        }
    }

    bot.onCommand("/echo") { msg, opts ->
        //bot.sendMessage(msg.chat.id, "${msg.text} ${opts ?: ""}")
        bot.sendMessage(msg.chat.id, "echo ${opts ?: ""}")
    }

    bot.start()
}