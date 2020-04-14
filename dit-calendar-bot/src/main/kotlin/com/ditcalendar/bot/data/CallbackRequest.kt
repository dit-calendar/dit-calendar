package com.ditcalendar.bot.data

sealed class CallbackRequest

class UnassignCallbackRequest(val taskId: Long) : CallbackRequest()

fun parse(callbaBackData: String): CallbackRequest? =
        if (callbaBackData.startsWith("unassign")) {
            val taskId: Long? = callbaBackData.substringAfter("_").toLongOrNull()
            if (taskId != null)
                UnassignCallbackRequest(taskId)
            else
                null
        } else
            null
