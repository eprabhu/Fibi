package com.polus.fibicomp.claims.claimsIntegration.excelity.service;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.claims.claimsIntegration.excelity.pojo.SftpConfigurationData;

@Transactional
@Service(value = "sftpConfigurationSerive")
public class SftpConfigurationServiceImpl implements SftpConfigurationService {

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public SftpConfigurationData getSFTPConfigurationValue(String parameterName) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SftpConfigurationData> query = builder.createQuery(SftpConfigurationData.class);
		Root<SftpConfigurationData> sftpConfigurationData = query.from(SftpConfigurationData.class);
		Predicate predicateConfigurationKey = builder.equal(sftpConfigurationData.get("configurationKey"), parameterName);
		query.where(builder.and(predicateConfigurationKey));
		return session.createQuery(query).uniqueResult();
	}
	
	@Override
	public String getSftpConfigurationValueAsString(String parameterName) {
		SftpConfigurationData getSftpConfiguration = getSFTPConfigurationValue(parameterName);
		return getSftpConfiguration != null ? getSftpConfiguration.getConfigurationValue() : null;
	}
}
