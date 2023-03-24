package com.polus.fibicomp.applicationexception.service;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.applicationexception.dao.ApplicationExceptionDao;
import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.applicationexception.pojo.ApplicationErrorDetails;
import com.polus.fibicomp.security.AuthenticatedUser;

@Service(value = "applicationExceptionService")
public class ApplicationExceptionSeviceImpl implements ApplicationExceptionService {

	protected static Logger logger = LogManager.getLogger(ApplicationExceptionSeviceImpl.class.getName());

	@Autowired
	private ApplicationExceptionDao applicationExceptionDao;

	@Override
	public Object saveErrorDetails(ApplicationException applicationException, HttpServletRequest request) {
		ApplicationErrorDetails applicationErrorDetails = applicationException.getApplicationErrorDetails();
		if (applicationException.getE() != null) {
			setTheErrorDetails(applicationErrorDetails, applicationException.getE());
		}
		setTheRequestDetails(applicationErrorDetails, request);
		applicationExceptionDao.saveErrorDetails(applicationErrorDetails);
		return applicationErrorDetails;
	}

	private void setTheErrorDetails(ApplicationErrorDetails applicationErrorDetails, Exception ex) {
		StringWriter sw = new StringWriter();
		PrintWriter pw = new PrintWriter(sw);
		try {
			applicationErrorDetails.setDebugMessage(ex.getLocalizedMessage());
			ex.printStackTrace(pw);
			applicationErrorDetails.setStackTrace(sw.toString());
		} catch (Exception e) {
			logger.info("exception occured in setTheErrorDetails : {}", e.getMessage());
		} finally {
			try {
				sw.close();
				pw.close();
			} catch (IOException e) {
				logger.error("exception occured while closing the print and string writer : {}", e.getMessage());
			}
		}
		
	}

	private void setTheRequestDetails(ApplicationErrorDetails applicationErrorDetails, HttpServletRequest request) {
		applicationErrorDetails.setApiRequest(request.getRequestURI());
		applicationErrorDetails.setMethod(request.getMethod());
		applicationErrorDetails.setRequesterPersonId(AuthenticatedUser.getLoginPersonId());
		try {
		applicationErrorDetails.setRequestBody(request.getParameterMap().entrySet()
                .stream()
                .map(e -> e.getKey() + "=" + String.join(", ", e.getValue()))
                .collect(Collectors.joining(" ")));
			if (applicationErrorDetails.getRequestBody()!=null && "".equals(applicationErrorDetails.getRequestBody()))
				applicationErrorDetails.setRequestBody(request.getReader().lines().collect(Collectors.joining(System.lineSeparator())));
		} catch (Exception e) {
			logger.error("exception occured in setTheRequestDetails : {}", e.getMessage());
			e.printStackTrace();
		}
	}

}
