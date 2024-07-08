package com.polus.kcintegration.exception.global;

import java.net.ConnectException;
import java.time.LocalDateTime;

import org.hibernate.exception.ConstraintViolationException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.reactive.function.client.WebClientRequestException;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import com.polus.kcintegration.exception.custom.IntegrationCustomException;
import com.polus.kcintegration.exception.service.IntegrationExceptionLogService;
import com.polus.kcintegration.exception.vo.ExceptionResponse;

import jakarta.persistence.EntityNotFoundException;

@Order(Ordered.HIGHEST_PRECEDENCE)
@ControllerAdvice
public class GlobalExceptionHandler extends ResponseEntityExceptionHandler {

	@Autowired
	private IntegrationExceptionLogService integrationExceptionLogService;

	@ExceptionHandler(IntegrationCustomException.class)
	public final ResponseEntity<ExceptionResponse> handleCustomException(IntegrationCustomException ex, WebRequest request) {
		return createResponseEntity(ex, HttpStatus.BAD_REQUEST, "CUSTOM_ERROR");
	}

	@ExceptionHandler(EntityNotFoundException.class)
	public final ResponseEntity<ExceptionResponse> handleEntityNotFoundException(EntityNotFoundException ex,
			WebRequest request) {
		return createResponseEntity(ex, HttpStatus.NOT_FOUND, "ENTITY_NOT_FOUND");
	}

	@ExceptionHandler(ConstraintViolationException.class)
	public final ResponseEntity<ExceptionResponse> handleConstraintViolationException(ConstraintViolationException ex, WebRequest request) {
		return createResponseEntity(ex, HttpStatus.BAD_REQUEST, "VALIDATION_ERROR");
	}

	@ExceptionHandler(ConnectException.class)
	public final ResponseEntity<ExceptionResponse> handleConnectException(ConnectException ex, WebRequest request) {
		return createResponseEntity(ex, HttpStatus.SERVICE_UNAVAILABLE, "NETWORK_ERROR");
	}

	@ExceptionHandler(WebClientRequestException.class)
	public final ResponseEntity<ExceptionResponse> handleWebClientRequestException(WebClientRequestException ex, WebRequest request) {
		return createResponseEntity(ex, HttpStatus.INTERNAL_SERVER_ERROR, "WEB_CLIENT_ERROR");
	}

	@ExceptionHandler(Exception.class)
	public final ResponseEntity<ExceptionResponse> handleAllExceptions(Exception ex, WebRequest request) {
		return createResponseEntity(ex, HttpStatus.INTERNAL_SERVER_ERROR, "INTERNAL_SERVER_ERROR");
	}

	private ResponseEntity<ExceptionResponse> createResponseEntity(Exception ex, HttpStatus status, String errorCode) {
		ExceptionResponse response = new ExceptionResponse();
		response.setErrorMessage(ex.getMessage());
		response.setErrorCode(errorCode);
		response.setTimestamp(LocalDateTime.now().toString());

		integrationExceptionLogService.logException(ex.getMessage(), ex.getClass().getName());

		return new ResponseEntity<>(response, status);
	}
}
