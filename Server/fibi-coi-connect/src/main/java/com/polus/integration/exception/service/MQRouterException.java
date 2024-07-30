package com.polus.integration.exception.service;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.scheduling.annotation.Async;

import com.polus.integration.exception.config.ApplicationContextProvider;
import com.polus.integration.exception.dao.MQExceptionLogRepository;
import com.polus.integration.exception.pojo.MQExceptionsLog;

public class MQRouterException  extends RuntimeException {

	private static final long serialVersionUID = 1L;

	private static final Logger LOGGER = LogManager.getLogger(MQRouterException.class.getName());

    private String queueMessage;
    private String sourceQueueName;
    private String destinationQueueName;
    private String queueExchange;
    private Integer moduleCode;
    private Integer subModuleCode;
    private String actionType;
    public String errorCode;
    private String errorMessage;
    private String messageId;
    private Throwable cause;
    @SuppressWarnings("unused")
	private String userName;
    @SuppressWarnings("unused")
	private String userType;

    public MQRouterException(String errorCode, String errorMessage, Throwable cause, String queueMessage, String sourceQueueName,
                             String destinationQueueName, String queueExchange,
                             Integer moduleCode, Integer subModuleCode, String actionType, String messageId) {
        super(errorMessage, cause);
        this.queueMessage = queueMessage;
        this.sourceQueueName = sourceQueueName;
        this.destinationQueueName = destinationQueueName;
        this.queueExchange = queueExchange;
        this.moduleCode = moduleCode;
        this.subModuleCode = subModuleCode;
        this.actionType = actionType;
        this.errorCode = errorCode;
        this.errorMessage = errorMessage;
        this.messageId = messageId;
        this.cause = cause;
        saveMQRouterException();
    }

    public MQRouterException(String errorCode, String errorMessage, Throwable cause, String queueMessage, String sourceQueueName, String destinationQueueName, String queueExchange,
                             Integer moduleCode, Integer subModuleCode, String actionType, String messageId, String userName, String userType) {
        super(errorMessage, cause);
        this.queueMessage = queueMessage;
        this.sourceQueueName = sourceQueueName;
        this.destinationQueueName = destinationQueueName;
        this.queueExchange = queueExchange;
        this.moduleCode = moduleCode;
        this.subModuleCode = subModuleCode;
        this.actionType = actionType;
        this.errorCode = errorCode;
        this.errorMessage = errorMessage;
        this.messageId = messageId;
        this.cause = cause;
        this.userName = userName;
        this.userType = userType;
        saveMQRouterException();
    }

    @Override
    public String toString() {
        return "MQRouterException{" +
                "queueMessage='" + queueMessage + '\'' +
                ", sourceQueueName='" + sourceQueueName + '\'' +
                ", queueExchange='" + queueExchange + '\'' +
                ", moduleCode=" + moduleCode +
                ", subModuleCode=" + subModuleCode +
                ", actionType='" + actionType + '\'' +
                ", errorCode='" + errorCode + '\'' +
                ", errorMessage='" + errorMessage + '\'' +
                ", messageId=" + messageId +
                '}';
    }


    @Async
    public void saveMQRouterException() {
        try {
            LOGGER.error(toString());
            MQExceptionsLog exceptionsLog = new MQExceptionsLog();
            exceptionsLog.setActionType(actionType);
            exceptionsLog.setModuleCode(moduleCode);
            exceptionsLog.setSubModuleCode(subModuleCode);
            exceptionsLog.setMessageId(messageId);
            exceptionsLog.setErrorCode(errorCode);
            exceptionsLog.setErrorMessage(errorMessage);
            exceptionsLog.setSourceQueueName(sourceQueueName);
            exceptionsLog.setDestinationQueueName(destinationQueueName);
            exceptionsLog.setQueueExchange(queueExchange);
            exceptionsLog.setQueueMessage(queueMessage);
            exceptionsLog.setStackTrace(cause != null ? ExceptionUtils.getStackTrace(cause) : null);
            MQExceptionLogRepository exceptionsRepository = getMqExceptionsService();
            exceptionsRepository.save(exceptionsLog);
            LOGGER.info("Exception log error id : {}", exceptionsLog.getId());
        } catch (Exception e) {
            LOGGER.error("Unable to save exception log : {}", e.getMessage());
        }
    }

    private static MQExceptionLogRepository getMqExceptionsService() {
        return ApplicationContextProvider.getApplicationContext().getBean(MQExceptionLogRepository.class);
    }
}
