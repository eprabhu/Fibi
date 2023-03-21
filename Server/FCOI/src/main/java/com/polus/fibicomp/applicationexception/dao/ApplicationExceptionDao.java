package com.polus.fibicomp.applicationexception.dao;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.applicationexception.pojo.ApplicationErrorDetails;

@Transactional
@Service
public interface ApplicationExceptionDao {

	void saveErrorDetails(ApplicationErrorDetails applicationErrorDetails);

}
