package com.polus.kcintegration.exception.service;

import java.time.LocalDateTime;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.kcintegration.exception.pojo.IntegrationExceptionLog;
import com.polus.kcintegration.exception.repository.IntegrationExceptionLogRepository;

@Service
public class IntegrationExceptionLogService {

	private static final Logger logger = LoggerFactory.getLogger(IntegrationExceptionLogService.class);

	@Autowired
	private IntegrationExceptionLogRepository integrationExceptionLogRepository;

	/**
	 * Logs an exception to the database.
	 *
	 * @param message The exception message.
	 * @param type    The type or category of the exception.
	 */
	@Transactional
	public void logException(String message, String type) {
		try {
			IntegrationExceptionLog log = new IntegrationExceptionLog();
			log.setExceptionMessage(message);
			log.setExceptionType(type);
			log.setTimestamp(LocalDateTime.now());
			integrationExceptionLogRepository.save(log);
		} catch (Exception e) {
			logger.error("Error occurred while logging exception: {}", e.getMessage());
			throw new RuntimeException("Failed to log exception", e);
		}
	}

}
