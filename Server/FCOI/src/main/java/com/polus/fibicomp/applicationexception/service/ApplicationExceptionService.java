package com.polus.fibicomp.applicationexception.service;

import javax.servlet.http.HttpServletRequest;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.applicationexception.dto.ApplicationException;

@Service
public interface ApplicationExceptionService {

	Object saveErrorDetails(ApplicationException ex, HttpServletRequest request);

}
