package com.ditcalendar.bot

import com.ditcalendar.bot.config.*
import com.ditcalendar.bot.data.OnlyText
import com.ditcalendar.bot.data.TelegramLink
import com.ditcalendar.bot.data.WithInline
import com.ditcalendar.bot.parsing.parseResponse
import com.elbekD.bot.Bot
import com.elbekD.bot.server
import com.elbekD.bot.types.InlineKeyboardButton
import com.elbekD.bot.types.InlineKeyboardMarkup

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
        val msgUser = msg.from
        //if message user is not set, we can't process
        if (msgUser == null) {
            bot.sendMessage(msg.chat.id, "fehlerhafte Anfrage")
        } else {
            val telegramLink = TelegramLink(msg.chat.id, msgUser.id, msgUser.username, msgUser.first_name)
            val response = calendarCommand.parseRequest(telegramLink, opts)
            val result = parseResponse(response)
            when (result) {
                is OnlyText -> bot.sendMessage(msg.chat.id, result.message, "MarkdownV2", true)
                is WithInline -> {
                    val inlineButton = InlineKeyboardButton(result.callBackText, callback_data = result.callBackData)
                    val inlineKeyboardMarkup = InlineKeyboardMarkup(listOf(listOf(inlineButton)))
                    bot.sendMessage(msg.chat.id, result.message, "MarkdownV2", true, markup = inlineKeyboardMarkup)
                }
            }

        }
    }

    bot.start()
}