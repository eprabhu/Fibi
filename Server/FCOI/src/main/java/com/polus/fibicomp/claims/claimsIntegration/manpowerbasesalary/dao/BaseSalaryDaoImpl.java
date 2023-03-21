package com.polus.fibicomp.claims.claimsIntegration.manpowerbasesalary.dao;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.claims.claimsIntegration.excelity.pojo.ClaimFiles;
import com.polus.fibicomp.manpower.pojo.ManpowerConfigurationData;

@Transactional
@Service(value = "baseSalaryDao")
public class BaseSalaryDaoImpl implements BaseSalaryDao {

	protected static Logger logger = LogManager.getLogger(BaseSalaryDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public ClaimFiles saveManpowerSftpFiles(ClaimFiles baseSalaryclaimFiles) {
		hibernateTemplate.save(baseSalaryclaimFiles);
		return baseSalaryclaimFiles;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public Set<String> getpersonIdByFileId(Integer fileId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Set<String> employeeIds = new HashSet();
		String personId = "SELECT distinct manpowerPersonId FROM Manpower WHERE fileId = :fileId";
		Query query = session.createQuery(personId);
		query.setParameter("fileId", fileId);
		List employeeData = query.getResultList();
		if (employeeData != null) {
			employeeIds.addAll(employeeData);
		}
		return employeeIds;
	}

	@Override
	public ManpowerConfigurationData getManpowerConfigurationValue(String parameterName) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ManpowerConfigurationData> query = builder.createQuery(ManpowerConfigurationData.class);
		Root<ManpowerConfigurationData> manpowerConfigurationKey = query.from(ManpowerConfigurationData.class);
		Predicate predicateConfigurationKey = builder.equal(manpowerConfigurationKey.get("configurationKey"),
				parameterName);
		query.where(builder.and(predicateConfigurationKey));
		return session.createQuery(query).uniqueResult();
	}

}
