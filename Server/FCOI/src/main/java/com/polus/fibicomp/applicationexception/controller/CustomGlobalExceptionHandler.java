package com.polus.fibicomp.applicationexception.controller;

import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.applicationexception.service.ApplicationExceptionService;

@Order(Ordered.HIGHEST_PRECEDENCE)
@ControllerAdvice
public class CustomGlobalExceptionHandler extends ResponseEntityExceptionHandler {

	@Autowired
	private ApplicationExceptionService applicationExceptionService;

	@Override
	protected ResponseEntity<Object> handleMethodArgumentNotValid(MethodArgumentNotValidException ex, HttpHeaders headers, HttpStatus status, WebRequest request) {
		Map<String, String> errors = new HashMap<>();
		ex.getBindingResult().getAllErrors().forEach((error) -> {
			String fieldName = ((FieldError) error).getField();
			String errorMessage = error.getDefaultMessage();
			errors.put(fieldName, errorMessage);
		});
		return new ResponseEntity<>(errors, HttpStatus.BAD_REQUEST);
	}

	@ExceptionHandler(ApplicationException.class)
	protected ResponseEntity<Object> handleApplicationException(ApplicationException ex, WebRequest request, HttpServletRequest servletRequest) {
		return new ResponseEntity<>(applicationExceptionService.saveErrorDetails(ex, servletRequest), HttpStatus.INTERNAL_SERVER_ERROR);
	}

	@ExceptionHandler(RuntimeException.class)
	protected ResponseEntity<Object> handleRuntimeException(RuntimeException ex, WebRequest request, HttpServletRequest servletRequest) {
		ApplicationException applicationException = new ApplicationException("oops! something went wrong please try again", ex, "handleRuntimeException");
		return new ResponseEntity<>(applicationExceptionService.saveErrorDetails(applicationException, servletRequest), HttpStatus.INTERNAL_SERVER_ERROR);
	}

	@ExceptionHandler(Exception.class)
	protected ResponseEntity<Object> handleException(Exception ex, WebRequest request, HttpServletRequest servletRequest) {
		ApplicationException applicationException = new ApplicationException("oops! something went wrong please try again", ex, "handlesException");
		return new ResponseEntity<>(applicationExceptionService.saveErrorDetails(applicationException, servletRequest), HttpStatus.INTERNAL_SERVER_ERROR);
	}

}
