package com.polus.fibicomp.claims.claimsIntegration.excelity.dao;

import java.util.HashSet;
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
import com.polus.fibicomp.manpower.pojo.AwardManpowerPayroll;
import com.polus.fibicomp.manpower.pojo.ManpowerConfigurationData;

@Transactional
@Service(value = "manpowerSftpDao")
public class ExcelityDaoImpl implements ExcelityDao {

	protected static Logger logger = LogManager.getLogger(ExcelityDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	public AwardManpowerPayroll saveManpowerSftp(AwardManpowerPayroll awardManpowerPayroll) {
		hibernateTemplate.save(awardManpowerPayroll);
		return awardManpowerPayroll;
	}

	@Override
	public ClaimFiles saveManpowerSftpFiles(ClaimFiles excelityClaimFiles) {
		hibernateTemplate.save(excelityClaimFiles);
		return excelityClaimFiles;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public Set<String> getaccountCodeByFileId(Integer fileId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Set<String> accountCode = new HashSet();
		String awardCode = "SELECT distinct glAccountCode FROM AwardManpowerPayroll WHERE fileId = :fileId";
		Query query = session.createQuery(awardCode);
		query.setParameter("fileId", fileId);
		if (query.getResultList() != null && !query.getResultList().isEmpty()) {
			accountCode.addAll(query.getResultList());
		}
		return accountCode;
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
