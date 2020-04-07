package com.ditcalendar.bot

import com.ditcalendar.bot.config.*
import com.ditcalendar.bot.data.OnlyText
import com.ditcalendar.bot.data.TelegramLink
import com.ditcalendar.bot.data.WithInline
import com.ditcalendar.bot.data.core.Base
import com.ditcalendar.bot.formatter.parseResponse
import com.elbekD.bot.Bot
import com.elbekD.bot.server
import com.elbekD.bot.types.InlineKeyboardButton
import com.elbekD.bot.types.InlineKeyboardMarkup
import com.elbekD.bot.types.Message
import com.github.kittinunf.result.Result

val helpMessage =
        """
            Mögliche Befehle sind:
            /postcalendar {Hier Id einfügen}
        """.trimIndent()

fun main(args: Array<String>) {


    val config by config()

    val token = config[telegram_token]
    val herokuApp = config[heroku_app_name]
    val calendarService = DitCalendarService()

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

    bot.onCallbackQuery { callbackQuery ->
        val request = callbackQuery.data
        val originallyMessage = callbackQuery.message

        if (request == null || originallyMessage == null) {
            bot.answerCallbackQuery(callbackQuery.id, "fehlerhafte Anfrage")
        } else {
            val msgUser = callbackQuery.from
            val telegramLink = TelegramLink(originallyMessage.chat.id, msgUser.id, msgUser.username, msgUser.first_name)
            val response = calendarService.executeCallback(telegramLink, request)

            when (val result = parseResponse(response)) {
                is OnlyText -> {
                    bot.answerCallbackQuery(callbackQuery.id, "erfolgreich ausgetragen")
                    bot.editMessageText(originallyMessage.chat.id, originallyMessage.message_id, text = result.message,
                            parseMode = "MarkdownV2")
                }
                is WithInline ->
                    bot.answerCallbackQuery(callbackQuery.id, "nicht implementiert", true)
            }
        }
    }

    //for deeplinking
    bot.onCommand("/start") { msg, opts ->
        val msgUser = msg.from
        //if message user is not set, we can't process
        if (msgUser == null) {
            bot.sendMessage(msg.chat.id, "fehlerhafte Anfrage")
        } else {
            if (opts != null && opts.startsWith("assign")) {
                val telegramLink = TelegramLink(msg.chat.id, msgUser.id, msgUser.username, msgUser.first_name)
                val response = calendarService.executeTaskAssignmentCommand(telegramLink, opts)

                sendMessage(response, bot, msg)
            } else {

                bot.sendMessage(msg.chat.id, helpMessage)
            }
        }
    }

    bot.onCommand("/postcalendar") { msg, opts ->
        val msgUser = msg.from
        //if message user is not set, we can't process
        if (msgUser == null) {
            bot.sendMessage(msg.chat.id, "fehlerhafte Anfrage")
        } else {
            val response = calendarService.executePublishCalendarCommand(opts)
            sendMessage(response, bot, msg)
        }
    }

    bot.start()
}

private fun sendMessage(response: Result<Base, Exception>, bot: Bot, msg: Message) {
    when (val result = parseResponse(response)) {
        is OnlyText ->
            bot.sendMessage(msg.chat.id, result.message, "MarkdownV2", true)
        is WithInline -> {
            val inlineButton = InlineKeyboardButton(result.callBackText, callback_data = result.callBackData)
            val inlineKeyboardMarkup = InlineKeyboardMarkup(listOf(listOf(inlineButton)))
            bot.sendMessage(msg.chat.id, result.message, "MarkdownV2", true, markup = inlineKeyboardMarkup)
        }
    }
}