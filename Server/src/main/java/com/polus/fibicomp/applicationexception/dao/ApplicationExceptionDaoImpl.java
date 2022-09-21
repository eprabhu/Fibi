package com.polus.fibicomp.applicationexception.dao;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.applicationexception.pojo.ApplicationErrorDetails;

@Transactional
@Service(value = "applicationExceptionDao")
public class ApplicationExceptionDaoImpl implements ApplicationExceptionDao {

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public void saveErrorDetails(ApplicationErrorDetails applicationErrorDetails) {
		hibernateTemplate.save(applicationErrorDetails);
	}

}
