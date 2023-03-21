package com.polus.fibicomp.compilance.dao;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

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

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.compilance.pojo.AcProtocol;
import com.polus.fibicomp.compilance.pojo.AcProtocolStatus;
import com.polus.fibicomp.compilance.pojo.IrbProtocol;
import com.polus.fibicomp.compilance.pojo.IrbProtocolStatus;
import com.polus.fibicomp.compilance.pojo.SpecialReviewUsage;
import com.polus.fibicomp.compilance.vo.ProtocolVO;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.pojo.SpecialReviewType;

@Transactional
@Service(value = "complianceDao")
public class ComplianceDaoImpl implements ComplianceDao {

	protected static Logger logger = LogManager.getLogger(ComplianceDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public List<SpecialReviewType> fetchAllSpecialReviewType() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SpecialReviewType> query = builder.createQuery(SpecialReviewType.class);
		Root<SpecialReviewType> rootSpecialReviewType = query.from(SpecialReviewType.class);
		query.orderBy(builder.asc(rootSpecialReviewType.get("sortId")));
		List<SpecialReviewType> specialReviewTypes = session.createQuery(query).getResultList();
		return specialReviewTypes;
	}

	@Override
	public List<SpecialReviewUsage> fetchSpecialReviewUsageByModuleCode(String moduleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SpecialReviewUsage> query = builder.createQuery(SpecialReviewUsage.class);
		Root<SpecialReviewUsage> rootSpecialReviewUsage = query.from(SpecialReviewUsage.class);
		Predicate predicate1 = builder.equal(rootSpecialReviewUsage.get("moduleCode"), moduleCode);
		Predicate predicate2 = builder.equal(rootSpecialReviewUsage.get("active"), true);
		query.where(builder.and(predicate1, predicate2));
		List<SpecialReviewUsage> specialReviewUsages = session.createQuery(query).getResultList();
		return specialReviewUsages;
	}

	@Override
	public List<IrbProtocol> loadIrbProtocolDetail(ProtocolVO vo) {
		try {
			StringBuilder titleLikeCriteria = new StringBuilder();
			StringBuilder protocolNumberLikeCriteria = new StringBuilder();
			Predicate predicatePersonId = null;
			Predicate predicateExpirationDate = null;
			Timestamp endDate = null ;
			Timestamp startDay = null;
			if (vo.getTitle() != null && !vo.getTitle().isEmpty()) {
				titleLikeCriteria = titleLikeCriteria.append("%").append(vo.getTitle()).append("%");
			}
			if (vo.getProtocolNumber() != null && !vo.getProtocolNumber().isEmpty()) {
				protocolNumberLikeCriteria = protocolNumberLikeCriteria.append("%").append(vo.getProtocolNumber()).append("%");
			}
			if (vo.getExpirationDate() != null && !vo.getExpirationDate().toString().isEmpty()) {
				endDate = Timestamp.valueOf((vo.getExpirationDate().toString().substring(0, 11)).concat(Constants.END_TIME));
				startDay = Timestamp.valueOf((vo.getExpirationDate().toString().substring(0, 11)).concat(Constants.START_TIME));
			}
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<IrbProtocol> query = builder.createQuery(IrbProtocol.class);
			Root<IrbProtocol> irbProtocol = query.from(IrbProtocol.class);
			List<Predicate> inRestrictions = new ArrayList<>();
			Predicate predicateTitle = builder.like(irbProtocol.get("title"), titleLikeCriteria.toString());
			Predicate predicateProtocolNumber = builder.like(irbProtocol.get("protocolNumber"), protocolNumberLikeCriteria.toString());
			Predicate predicateFundingSource = builder.equal(irbProtocol.get("fundingSourceTypeCode"), vo.getFundingSourceTypeCode());
			if (vo.getExpirationDate() != null && !vo.getExpirationDate().toString().isEmpty()) {
				predicateExpirationDate = builder.between(irbProtocol.get("expirationDate"), startDay, endDate);
			}
			Predicate predicateProtocolStatus = builder.equal(irbProtocol.get("protocolStatusCode"), vo.getProtocolStatusCode());
			predicatePersonId = builder.equal(irbProtocol.get("personId"), vo.getPersonId());
			if (vo.getTitle() != null && !vo.getTitle().isEmpty()) {
				inRestrictions.add(predicateTitle);
			}
			if (vo.getProtocolNumber() != null && !vo.getProtocolNumber().isEmpty()) {
				inRestrictions.add(predicateProtocolNumber);
			}
			if (vo.getFundingSourceTypeCode() != null && !vo.getFundingSourceTypeCode().isEmpty()) {
				inRestrictions.add(predicateFundingSource);
			}
			if (predicateExpirationDate !=  null && vo.getExpirationDate() != null && !vo.getExpirationDate().toString().isEmpty()) {
				inRestrictions.add(predicateExpirationDate);
			}
			if (vo.getProtocolStatusCode() != null && !vo.getProtocolStatusCode().isEmpty()) {
				inRestrictions.add(predicateProtocolStatus);
			}
			if (predicatePersonId != null && vo.getPersonId() != null && !vo.getPersonId().isEmpty()) {
				inRestrictions.add(predicatePersonId);
			}
			query.where(builder.and(inRestrictions.toArray(new Predicate[inRestrictions.size()])));
			query.orderBy(builder.desc(irbProtocol.get("createTimestamp")));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			logger.error("Error in loadIrbProtocolDetail {}", e.getMessage());
			return Collections.emptyList();
		}
	}

	@Override
	public List<AcProtocol> loadAcProtocolDetail(ProtocolVO vo) {
		try {
			StringBuilder titleLikeCriteria = new StringBuilder();
			StringBuilder protocolNumberLikeCriteria = new StringBuilder();
			Timestamp endDate = null ;
			Timestamp startDay = null;
			Predicate predicatePersonId = null;
			Predicate predicateExpirationDate = null;
			if (vo.getTitle() != null && !vo.getTitle().isEmpty()) {
				titleLikeCriteria = titleLikeCriteria.append("%").append(vo.getTitle()).append("%");
			}
			if (vo.getProtocolNumber() != null && !vo.getProtocolNumber().isEmpty()) {
				protocolNumberLikeCriteria = protocolNumberLikeCriteria.append("%").append(vo.getProtocolNumber()).append("%");
			}
			if (vo.getExpirationDate() != null && !vo.getExpirationDate().toString().isEmpty()) {
				endDate = Timestamp.valueOf((vo.getExpirationDate().toString().substring(0, 11)).concat(Constants.END_TIME));
				startDay = Timestamp.valueOf((vo.getExpirationDate().toString().substring(0, 11)).concat(Constants.START_TIME));
			}
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AcProtocol> query = builder.createQuery(AcProtocol.class);
			Root<AcProtocol> acProtocol = query.from(AcProtocol.class);
			List<Predicate> inRestrictions = new ArrayList<>();
			Predicate predicateTitle = builder.like(acProtocol.get("title"), titleLikeCriteria.toString());
			Predicate predicateProtocolNumber = builder.like(acProtocol.get("protocolNumber"), protocolNumberLikeCriteria.toString());
			Predicate predicateFundingSource = builder.equal(acProtocol.get("fundingSourceTypeCode"),vo.getFundingSourceTypeCode());
			if (vo.getExpirationDate() != null && !vo.getExpirationDate().toString().isEmpty()) {
				predicateExpirationDate = builder.between(acProtocol.get("expirationDate"), startDay, endDate);
			}
			Predicate predicateProtocolStatus = builder.equal(acProtocol.get("protocolStatusCode"), vo.getProtocolStatusCode());
			predicatePersonId = builder.equal(acProtocol.get("personId"), vo.getPersonId());
			if (vo.getTitle() != null && !vo.getTitle().isEmpty()) {
				inRestrictions.add(predicateTitle);
			}
			if (vo.getProtocolNumber() != null && !vo.getProtocolNumber().isEmpty()) {
				inRestrictions.add(predicateProtocolNumber);
			}
			if (vo.getFundingSourceTypeCode() != null && !vo.getFundingSourceTypeCode().isEmpty()) {
				inRestrictions.add(predicateFundingSource);
			}
			if (predicateExpirationDate != null && vo.getExpirationDate() != null && !vo.getExpirationDate().toString().isEmpty()) {
				inRestrictions.add(predicateExpirationDate);
			}
			if (vo.getProtocolStatusCode() != null && !vo.getProtocolStatusCode().isEmpty()) {
				inRestrictions.add(predicateProtocolStatus);
			}
			if (predicatePersonId != null && vo.getPersonId() != null && !vo.getPersonId().isEmpty()) {
				inRestrictions.add(predicatePersonId);
			}
			query.where(builder.and(inRestrictions.toArray(new Predicate[inRestrictions.size()])));
			query.orderBy(builder.desc(acProtocol.get("createTimestamp")));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			logger.error("Error in loadAcProtocolDetail {}", e.getMessage());
			return Collections.emptyList();
		}
	}

	@Override
	public IrbProtocol fetchLatestIrbPrtocol(String protocolNumber) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("From IrbProtocol where protocolNumber = :protocolNumber and protocolId = (select max(protocolId) from IrbProtocol where protocolNumber = :protocolNumber)");
			Query queryIrbProtocol = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			queryIrbProtocol.setParameter("protocolNumber", protocolNumber);
			return (IrbProtocol) queryIrbProtocol.getSingleResult();
		} catch (Exception e) {
			logger.error("error in fetchLatestIrbPrtocol{}", e.getMessage());
			throw new ApplicationException("Error occurred in fetchLatestIrbPrtocol", e, Constants.JAVA_ERROR);
		}
	}

	@Override
	public AcProtocol fetchLatestAcPrtocol(String protocolNumber) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("From AcProtocol where protocolNumber = :protocolNumber and protocolId = (select max(protocolId) from AcProtocol where protocolNumber = :protocolNumber)");
			Query queryIrbProtocol = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			queryIrbProtocol.setParameter("protocolNumber", protocolNumber);
			return (AcProtocol) queryIrbProtocol.getSingleResult();
		} catch (Exception e) {
			logger.error("error in fetchLatestAcbPrtocol{}", e.getMessage());
			throw new ApplicationException("Error occurred in fetchLatestAcPrtocol", e, Constants.JAVA_ERROR);
		}
	}

	@Override
	public List<AcProtocolStatus> getAcProtocolStatus() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AcProtocolStatus> query = builder.createQuery(AcProtocolStatus.class);
		Root<AcProtocolStatus> rootAcProtocolStatus = query.from(AcProtocolStatus.class);
		Predicate predicateIsActive = builder.equal(rootAcProtocolStatus.get("isActive"), Boolean.TRUE);
		query.where(builder.and(predicateIsActive));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<IrbProtocolStatus> getIrbProtocolStatus() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<IrbProtocolStatus> query = builder.createQuery(IrbProtocolStatus.class);
		Root<IrbProtocolStatus> rootIrbProtocolStatus = query.from(IrbProtocolStatus.class);
		Predicate predicateIsActive = builder.equal(rootIrbProtocolStatus.get("isActive"), Boolean.TRUE);
		query.where(builder.and(predicateIsActive));
		return session.createQuery(query).getResultList();
	}

}
