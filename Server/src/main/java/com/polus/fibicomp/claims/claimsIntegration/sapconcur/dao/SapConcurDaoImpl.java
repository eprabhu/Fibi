package com.polus.fibicomp.claims.claimsIntegration.sapconcur.dao;

import java.util.List;

import javax.persistence.Query;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.claims.claimsIntegration.excelity.pojo.ClaimFiles;
import com.polus.fibicomp.claims.claimsIntegration.sapconcur.pojo.ConcurStaffTravelDtls;

@Transactional
@Service(value = "sapConcurDao")
public class SapConcurDaoImpl implements SapConcurDao {

	protected static Logger logger = LogManager.getLogger(SapConcurDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public void saveSapConcurSftp(ConcurStaffTravelDtls concurStaffTravelDtls) {
		hibernateTemplate.saveOrUpdate(concurStaffTravelDtls);
	}

	@Override
	public ClaimFiles saveSapConcurSftpFiles(ClaimFiles concurClaimFiles) {
		hibernateTemplate.save(concurClaimFiles);
		return concurClaimFiles;
	}

	@SuppressWarnings({ "unchecked" })
	@Override
	public List<String> getaccountCodeByFileId(Integer fileId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String queryStmt = "SELECT distinct concurReferenceNumber FROM ConcurStaffTravelDtls WHERE fileId = :fileId";
		Query query = session.createQuery(queryStmt);
		query.setParameter("fileId", fileId);
		return query.getResultList();
	}
}
