package com.polus.fibicomp.externalreviewer.dao;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerAcademicArea;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerAcademicRank;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerAffilation;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerAttachment;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerAttachmentFile;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerCira;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerOriginality;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerThoroughness;
import com.polus.fibicomp.externalreviewer.pojo.ExternalReviewer;
import com.polus.fibicomp.externalreviewer.pojo.ExternalReviewerAttachmentType;
import com.polus.fibicomp.externalreviewer.pojo.ExternalReviewerExt;
import com.polus.fibicomp.externalreviewer.pojo.ExternalReviewerRights;
import com.polus.fibicomp.externalreviewer.pojo.ExternalReviewerSpecialization;
import com.polus.fibicomp.externalreviewer.pojo.ReviewerRights;
import com.polus.fibicomp.externalreviewer.pojo.SpecialismKeyword;
import com.polus.fibicomp.externalreviewer.vo.ExternalReviewerVo;
import com.polus.fibicomp.pojo.Country;
import com.polus.fibicomp.security.AuthenticatedUser;


@Transactional
@Service(value = "externalReviewerDao")
public class ExternalReviewerDaoImpl implements ExternalReviewerDao {

	protected static Logger logger = LogManager.getLogger(ExternalReviewerDaoImpl.class.getName());
	
	private static final String EXTERNAL_REVIEWER_ID = "externalReviewerId";
	private static final String UPDATE_TIMESTAMP = "updateTimeStamp";
	private static final String PASSPORTNAME ="passportName";
	private static final String PRIMARY_EMAIL ="primaryEmail";
	private static final String AGREEMENT_START_DATE ="agreementStartDate";
	private static final String AGREEMENT_END_DATE ="agreementEndDate";
	private static final String STATUS ="status";         
	private static final String COUNTRY_CODE ="countryCode";
	@Autowired
	private HibernateTemplate hibernateTemplate;
	
	@Autowired
	private CommonDao commonDao;
	
	@Override
	public ExternalReviewer saveOrUpdateExtReviewer(ExternalReviewer extReviewer) {
		hibernateTemplate.saveOrUpdate(extReviewer);
		return extReviewer;
	}
	
	@Override
	public ExternalReviewerExt saveOrUpdateExternalReviewerExt(ExternalReviewerExt externalReviewerExt) {
		hibernateTemplate.saveOrUpdate(externalReviewerExt);
		return externalReviewerExt;
	}
	
	@Override
	public ExternalReviewerSpecialization saveOrUpdateExtReviewerSpecialization(ExternalReviewerSpecialization extReviewerSpecialization) {
		hibernateTemplate.saveOrUpdate(extReviewerSpecialization);
		return extReviewerSpecialization;
	}
	
	@Override
	public ExternalReviewerRights saveOrUpdateExternalReviewerRights(ExternalReviewerRights externalReviewerRight) {
		hibernateTemplate.saveOrUpdate(externalReviewerRight);
		return externalReviewerRight;
	}
	
	@Override
	public ExtReviewerAttachment saveOrUpdateExtAttachment(ExtReviewerAttachment externalReviewerAttachment) {
		hibernateTemplate.saveOrUpdate(externalReviewerAttachment);
		return externalReviewerAttachment;
	}
	
	@Override
	public ExternalReviewer getExtReviewerDetailById(Integer extReviewerId) {
		return hibernateTemplate.get(ExternalReviewer.class, extReviewerId);
	}

	@Override
	public ExternalReviewerExt getExternalReviewerExts(Integer extReviewerId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ExternalReviewerExt> query = builder.createQuery(ExternalReviewerExt.class);
		Root<ExternalReviewerExt> rootExternalReviewerExt = query.from(ExternalReviewerExt.class);
		query.where(builder.equal(rootExternalReviewerExt.get(EXTERNAL_REVIEWER_ID), extReviewerId));
		query.orderBy(builder.desc(rootExternalReviewerExt.get(UPDATE_TIMESTAMP)));
		return session.createQuery(query).uniqueResult();
	}
	
	@Override
	public List<ExternalReviewerSpecialization> getExternalReviewerSpecializations(Integer extReviewerId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ExternalReviewerSpecialization> query = builder.createQuery(ExternalReviewerSpecialization.class);
		Root<ExternalReviewerSpecialization> rootExternalReviewerExt = query.from(ExternalReviewerSpecialization.class);
		query.where(builder.equal(rootExternalReviewerExt.get(EXTERNAL_REVIEWER_ID), extReviewerId));
		query.orderBy(builder.desc(rootExternalReviewerExt.get(UPDATE_TIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ExternalReviewerRights getExternalReviewerRights(Integer extReviewerId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ExternalReviewerRights> query = builder.createQuery(ExternalReviewerRights.class);
		Root<ExternalReviewerRights> rootExternalReviewerExt = query.from(ExternalReviewerRights.class);
		query.where(builder.equal(rootExternalReviewerExt.get(EXTERNAL_REVIEWER_ID), extReviewerId));
		query.orderBy(builder.desc(rootExternalReviewerExt.get(UPDATE_TIMESTAMP)));
		return session.createQuery(query).uniqueResult();
	}
	
	@Override
	public String getexternalReviewerPassword(Integer externalReviewerId) {
	    Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<String> query = builder.createQuery(String.class);
		Root<ExternalReviewer> rootExternalReviewerExt = query.from(ExternalReviewer.class);
		query.where(builder.equal(rootExternalReviewerExt.get(EXTERNAL_REVIEWER_ID), externalReviewerId));
		query.select(rootExternalReviewerExt.get("password"));
		return session.createQuery(query).uniqueResult();
	}
	
	@Override
	public boolean checkUniqueUserName(String principalName) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<String> query = builder.createQuery(String.class);
		Root<ExternalReviewer> rootExternalReviewerExt = query.from(ExternalReviewer.class);
		query.where(builder.equal(rootExternalReviewerExt.get("principalName"), principalName));
		query.select(rootExternalReviewerExt.get("principalName"));
		if (session.createQuery(query).uniqueResult() != null) {
			return true;
		}
		return false;
	}
	
	@Override
	public List<ExternalReviewerAttachmentType> fetchExternalReviewerAttachmentTypes() {
		return hibernateTemplate.loadAll(ExternalReviewerAttachmentType.class);
	}

	@Override
	public List<ExtReviewerAcademicArea> fetchExtReviewerAcademicArea() {
		return hibernateTemplate.loadAll(ExtReviewerAcademicArea.class);
	}
	
	@Override
	public List<ExtReviewerAcademicRank> fetchExtReviewerAcademicRank() {
		return hibernateTemplate.loadAll(ExtReviewerAcademicRank.class);
	}
	
	@Override
	public List<ExtReviewerAffilation> fetchExtReviewerAffilation() {
		return hibernateTemplate.loadAll(ExtReviewerAffilation.class);
	}
	
	@Override
	public List<ExtReviewerCira> fetchExtReviewerCira() {
		return hibernateTemplate.loadAll(ExtReviewerCira.class);
	}
	
	@Override
	public List<ExtReviewerOriginality> fetchExtReviewerOriginality() {
		return hibernateTemplate.loadAll(ExtReviewerOriginality.class);
	}
	
	@Override
	public List<ExtReviewerThoroughness> fetchExtReviewerThoroughness() {
		return hibernateTemplate.loadAll(ExtReviewerThoroughness.class);
	}
	
	@Override
	public List<ReviewerRights> fetchReviewerRights() {
		return hibernateTemplate.loadAll(ReviewerRights.class);
	}
	
	@Override
	public ExtReviewerAttachmentFile saveFileData(ExtReviewerAttachmentFile fileData) {
		hibernateTemplate.save(fileData);
		return fileData;
	}
	
	@Override
	public ExtReviewerAttachment getExtReviewerAttachmentById(Integer externalReviewerAttachmentId) {
		return hibernateTemplate.get(ExtReviewerAttachment.class, externalReviewerAttachmentId);
	}

	@Override
	public ExtReviewerAttachmentFile getFileDataById(String fileDataId) {
		return hibernateTemplate.get(ExtReviewerAttachmentFile.class, fileDataId);
	}
	
	@Override
	public void deleteExtReviewerSpecialization(Integer extReviewerSpecializationId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<ExternalReviewerSpecialization> delete = builder.createCriteriaDelete(ExternalReviewerSpecialization.class);
		Root<ExternalReviewerSpecialization> root = delete.from(ExternalReviewerSpecialization.class);
		delete.where(builder.equal(root.get("extReviewerSpecializationId"), extReviewerSpecializationId));
		session.createQuery(delete).executeUpdate();
	}
	
	@Override
	public void deleteFileData(ExtReviewerAttachmentFile fileData) {
		hibernateTemplate.delete(fileData);
	}
	
	@Override
	public void deleteExtReviewerAttachment(ExtReviewerAttachment extReviewerAttachment) {
		hibernateTemplate.delete(extReviewerAttachment);
	}
	
	@Override
	public List<ExtReviewerAttachment> fetchExtReviewerAttachment(Integer externalReviewerId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ExtReviewerAttachment> query = builder.createQuery(ExtReviewerAttachment.class);
		Root<ExtReviewerAttachment> rootExternalReviewerAttachment = query.from(ExtReviewerAttachment.class);
		query.where(builder.equal(rootExternalReviewerAttachment.get(EXTERNAL_REVIEWER_ID), externalReviewerId));
		return session.createQuery(query).getResultList();
	}
	
	@Override
	public void updateExtAttachment(String description, Integer externalReviewerAttachmentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder criteriaBuilder = session.getCriteriaBuilder();
		CriteriaUpdate<ExtReviewerAttachment> criteriaUpdate = criteriaBuilder.createCriteriaUpdate(ExtReviewerAttachment.class);
		Root<ExtReviewerAttachment> root = criteriaUpdate.from(ExtReviewerAttachment.class);
		criteriaUpdate.set("description", description);
		criteriaUpdate.set("updateTimestamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName());
		criteriaUpdate.where(criteriaBuilder.equal(root.get("externalReviewerAttachmentId"),externalReviewerAttachmentId));		 		
		session.createQuery(criteriaUpdate).executeUpdate();
	}
	
	@Override
	public ExternalReviewerVo getAllExtReviewer(ExternalReviewerVo vo) {
		Integer pageNumber  = vo.getPageNumber();
		Integer currentPage = vo.getCurrentPage();
		List<Order> orderList = new ArrayList<>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ExternalReviewer> query = builder.createQuery(ExternalReviewer.class);
		Root<ExternalReviewer> rootExtReviewer = query.from(ExternalReviewer.class);
		Join<ExternalReviewer, Country> join = rootExtReviewer.join("countryDetails",JoinType.LEFT);
		Join<ExternalReviewer, ExtReviewerAffilation> joinAffilation = rootExtReviewer.join("affilationInstitution",JoinType.LEFT);
		List<Predicate> inRestrictions = new ArrayList<>();
		Predicate predicateOne = builder.like(rootExtReviewer.get("firstName"), "%" + vo.getProperty1() + "%");
		Predicate predicateTwo = builder.like(rootExtReviewer.get("lastName"), "%" + vo.getProperty2() + "%");
		Predicate predicateThree = builder.like(rootExtReviewer.get(PASSPORTNAME), "%" + vo.getProperty3() + "%");
		Predicate predicateFour = builder.like(rootExtReviewer.get(PRIMARY_EMAIL), "%" + vo.getProperty4() + "%");
		Predicate predicateFive = builder.like(rootExtReviewer.get(EXTERNAL_REVIEWER_ID).as(String.class),"%" + vo.getProperty6() + "%");
		Predicate predicate7 = builder.like(rootExtReviewer.get(AGREEMENT_START_DATE).as(String.class),vo.getProperty5() + "%");
		Predicate predicate6 = builder.like(rootExtReviewer.get(AGREEMENT_END_DATE).as(String.class), vo.getProperty7() + "%");
		Predicate predicate8 = builder.like(rootExtReviewer.get(STATUS),"%" + vo.getProperty8()+ "%");
		Predicate predicate9 = builder.like(join.get(COUNTRY_CODE),"%" +vo.getProperty9()+ "%");
		Predicate predicate10 = builder.in(rootExtReviewer.get("academicAreaCodeSecondary")).value(vo.getProperty10());
		Predicate predicate11 = builder.in(rootExtReviewer.get("academicAreaCodePrimary")).value(vo.getProperty11());
		Predicate predicate12 = builder.in(joinAffilation.get("affilationInstitutionCode")).value(vo.getProperty12());
		if (!"".equals(vo.getProperty6())) {
			inRestrictions.add(predicateFive);
		} 
		if(!"".equals(vo.getProperty7())) {
			inRestrictions.add(predicate6);
		}
		if(!"".equals(vo.getProperty5())) {
			inRestrictions.add(predicate7);
		}
		if(!vo.getProperty11().isEmpty() ) {
			inRestrictions.add(predicate11);
		}
		if(!vo.getProperty10().isEmpty() ) {
			inRestrictions.add(predicate10);
		}
		if(!"".equals(vo.getProperty9())) {
			inRestrictions.add(predicate9);
		}
		if(!"".equals(vo.getProperty12())) {
			inRestrictions.add(predicate12);
		}
		if(!"".equals(vo.getProperty1())) {
			inRestrictions.add(predicateOne);
		}
		if(!"".equals(vo.getProperty2())) {
			inRestrictions.add(predicateTwo);
		}
		if(!"".equals(vo.getProperty3())) {
			inRestrictions.add(predicateThree);
		}
		if(!"".equals(vo.getProperty4())) {
			inRestrictions.add(predicateFour);
		}
		if(!"".equals(vo.getProperty8())) {
			inRestrictions.add(predicate8);
		}
		query.where(builder.and(inRestrictions.toArray(new Predicate[inRestrictions.size()])));
		if (vo.getSort().isEmpty()) {
			orderList.add(builder.desc(generateExpression(rootExtReviewer, vo.getSortBy())));
		} else {
			for (Map.Entry<String, String> mapElement : vo.getSort().entrySet()) {
				if (mapElement.getValue().equals("desc")) {
					    orderList.add(builder.desc(generateExpression(rootExtReviewer, mapElement.getKey())));
				} else {
					   orderList.add(builder.asc(generateExpression(rootExtReviewer, mapElement.getKey())));
				}
			}
		}
		query.orderBy(orderList);
		int count = pageNumber * (currentPage - 1);
		vo.setExtReviewers(session.createQuery(query).setFirstResult(count).setMaxResults(pageNumber).getResultList());
		vo.setTotalExtReviewer((session.createQuery(query).getResultList()).size());
		return vo;
	}
	
	public <R> Expression<R> generateExpression(Root<R> root, String key) {
		if (!key.contains(".")) {
			return root.get(key);
		}
		Integer index = key.indexOf('.');
		return root.get(key.substring(0, index)).get(key.substring(index + 1, key.length()));
	}

	@Override
	public List<SpecialismKeyword> findSpecialismKeywords(String searchString) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SpecialismKeyword> query = builder.createQuery(SpecialismKeyword.class);
		Root<SpecialismKeyword> rootScienceKeyword = query.from(SpecialismKeyword.class);
		Predicate isActive = builder.equal(rootScienceKeyword.get("isActive"), Boolean.TRUE);
		Predicate description = builder.like(builder.lower(rootScienceKeyword.get("description")), "%" + searchString.toLowerCase() + "%");
		query.where(builder.and(isActive, description));
		query.orderBy(builder.asc(rootScienceKeyword.get(Constants.DESCRIPTION)));
		return session.createQuery(query).setMaxResults(25).getResultList();
	}
}
