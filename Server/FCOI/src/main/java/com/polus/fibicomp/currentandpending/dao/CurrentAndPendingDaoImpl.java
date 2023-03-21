package com.polus.fibicomp.currentandpending.dao;

import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.query.Query;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.currentandpending.pojo.CPReportHeader;
import com.polus.fibicomp.currentandpending.pojo.CPReportProjectDetail;
import com.polus.fibicomp.currentandpending.pojo.CPReportProjectDetailExt;

@Transactional
@Service(value = "currentAndPendingDao")
public class CurrentAndPendingDaoImpl implements CurrentAndPendingDao {

	protected static Logger logger = LogManager.getLogger(CurrentAndPendingDaoImpl.class.getName());

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public CPReportHeader saveOrUpdateCPReportHeader(CPReportHeader cpReportHeader) {
		try {
			hibernateTemplate.saveOrUpdate(cpReportHeader);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateCPReportHeader: {} ", e.getMessage());
		}
		return cpReportHeader;
	}

	@Override
	public Boolean checkIfReportGenerated(String personId, String moduleItemId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CPReportHeader> query = builder.createQuery(CPReportHeader.class);
		Root<CPReportHeader> root = query.from(CPReportHeader.class);
		Predicate predicateModuleItemId = builder.equal(root.get("moduleItemId"), moduleItemId);
		Predicate predicatePersonId = builder.equal(root.get("personId"), personId);
		query.where(builder.and(predicateModuleItemId, predicatePersonId));
		if (!(session.createQuery(query).list()).isEmpty() && (session.createQuery(query).list()) != null) {
			return true;
		} else {
			return false;
		}
	}

	@Override
	public CPReportProjectDetail saveOrUpdateCPReportProjectDetail(CPReportProjectDetail cpReportProjectDetail) {
		try {
			hibernateTemplate.saveOrUpdate(cpReportProjectDetail);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateCPReportProjectDetail: {} ", e.getMessage());
		}
		return cpReportProjectDetail;
	}

	@Override
	public Integer getMaxVersionNumberByParams(String moduleItemId, String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
		Root<CPReportHeader> rootCPReportHeader = query.from(CPReportHeader.class);
		Predicate predicateModuleItemId = builder.equal(rootCPReportHeader.get("moduleItemId"), moduleItemId);
		Predicate predicatePersonId = builder.equal(rootCPReportHeader.get("personId"), personId);
		query.select(builder.max(rootCPReportHeader.get("versionNumber")));
		query.where(builder.and(predicateModuleItemId, predicatePersonId));
		if(session.createQuery(query).getSingleResult() != null) {
			return  session.createQuery(query).getSingleResult()+1;
		} else {
			return 1;
		}
	}

	@Override
	public CPReportProjectDetailExt saveOrUpdateCPReportProjectDetailExt(CPReportProjectDetailExt cpReportProjectDetailExt) {
		try {
			hibernateTemplate.saveOrUpdate(cpReportProjectDetailExt);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateCPReportProjectDetailExt: {} ", e.getMessage());
		}
		return cpReportProjectDetailExt;
	}

	@Override
	public CPReportHeader fetchCPReportHeaderDetailsById(Integer cpReportHeaderId) {
		return hibernateTemplate.get(CPReportHeader.class, cpReportHeaderId);
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<String> fetchPersonIdsByParams(Integer moduleCode, String moduleItemId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT distinct personId FROM CPReportHeader WHERE moduleCode=:moduleCode and moduleItemId=:moduleItemId";
		Query<String> query = session.createQuery(hqlQuery);
		query.setParameter("moduleCode", moduleCode);
		query.setParameter("moduleItemId", moduleItemId);
		return query.getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public CPReportHeader getCPReportHeaderOfMaxVersionOfPerson(Integer moduleCode, String moduleItemId, String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String maxHQLQuery = "SELECT MAX(versionNumber) FROM CPReportHeader WHERE moduleCode=:moduleCode and moduleItemId=:moduleItemId and personId=:personId";
		Query<Integer> countQuery = session.createQuery(maxHQLQuery);
		countQuery.setParameter("moduleCode", moduleCode);
		countQuery.setParameter("moduleItemId", moduleItemId);
		countQuery.setParameter("personId", personId);
		Integer maxVersionNumber = countQuery.uniqueResult();
		String hqlQuery = "FROM CPReportHeader WHERE moduleCode=:moduleCode and moduleItemId=:moduleItemId and personId=:personId and versionNumber=:versionNumber";
		Query<CPReportHeader> query = session.createQuery(hqlQuery);
		query.setParameter("moduleCode", moduleCode);
		query.setParameter("moduleItemId", moduleItemId);
		query.setParameter("personId", personId);
		query.setParameter("versionNumber", maxVersionNumber);
		return query.uniqueResult();
	}

	@Override
	public List<CPReportProjectDetail> getCPReportProjectDetailsByCPReportHeaderId(Integer cpReportHeaderId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CPReportProjectDetail> query = builder.createQuery(CPReportProjectDetail.class);
		Root<CPReportProjectDetail> rootProposalPerson = query.from(CPReportProjectDetail.class);
		query.where(builder.and(builder.equal(rootProposalPerson.get("cpReportHeader").get("cpReportHeaderId"), cpReportHeaderId)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public CPReportProjectDetailExt getCPReportProjectDetailExtById(Integer cpReportProjectDetailId) {
		return hibernateTemplate.get(CPReportProjectDetailExt.class, cpReportProjectDetailId);
	}

	@Override
	public CPReportProjectDetail getCPReportProjectDetailsById(Integer cpReportProjectDetailId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CPReportProjectDetail> query = builder.createQuery(CPReportProjectDetail.class);
		Root<CPReportProjectDetail> rootProposalPerson = query.from(CPReportProjectDetail.class);
		query.where(builder.and(builder.equal(rootProposalPerson.get("cpReportProjectDetailId"), cpReportProjectDetailId)));
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public List<CPReportHeader> fetchCPReportHeadersByParams(String personId, Boolean nonEmployeeFlag, Integer moduleCode, String moduleItemId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CPReportHeader> query = builder.createQuery(CPReportHeader.class);
		Root<CPReportHeader> rootCPReportHeader = query.from(CPReportHeader.class);
		Predicate predicatePersonId = builder.equal(rootCPReportHeader.get("personId"), personId);
		Predicate predicateNonEmployeeFlag = builder.equal(rootCPReportHeader.get("nonEmployeeFlag"), nonEmployeeFlag);
		Predicate predicateModuleCode = builder.equal(rootCPReportHeader.get("moduleCode"), moduleCode);
		Predicate predicateModuleItemId = builder.equal(rootCPReportHeader.get("moduleItemId"), moduleItemId);
		query.where(builder.and(predicatePersonId, predicateNonEmployeeFlag, predicateModuleCode, predicateModuleItemId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<Integer> getCPProjectDetailIdsByCPReportHeaderId(Integer cpReportHeaderId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
		Root<CPReportProjectDetail> rootReviewStop = query.from(CPReportProjectDetail.class);
		query.where(builder.and(builder.equal(rootReviewStop.get("cpReportHeader").get("cpReportHeaderId"), cpReportHeaderId)));
		query.select(rootReviewStop.get("cpReportProjectDetailId")).distinct(true);
		return session.createQuery(query).getResultList();
	}

	@Override
	public void deleteCPReportHeaders(List<CPReportHeader> cpReportHeaders) {
		hibernateTemplate.deleteAll(cpReportHeaders);
	}

	@Override
	public void deleteCPProjectDetailExt(CPReportProjectDetailExt cpReportProjectDetailExt) {
		try {
			hibernateTemplate.delete(cpReportProjectDetailExt);
		} catch (Exception e) {
			logger.error("Error occured in deleteCPProjectDetailExt : {}", e.getMessage());
		}
	}

	@Override
	public String deleteCPExternalProjectDetail(Integer cpReportProjectDetailId) {
		hibernateTemplate.delete(hibernateTemplate.get(CPReportProjectDetail.class, cpReportProjectDetailId));
		return "External project detail deleted successfully";
	}

}
