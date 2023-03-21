package com.polus.fibicomp.sectionwiseedit.dao;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Subquery;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.sectionwiseedit.pojo.ModuleVariableSection;
import com.polus.fibicomp.sectionwiseedit.pojo.SectionModule;
import com.polus.fibicomp.sectionwiseedit.pojo.SectionType;

@Transactional
@Service(value = "sectionWiseEditDao")
public class SectionWiseEditDaoImpl implements SectionWiseEditDao {

	protected static Logger logger = LogManager.getLogger(SectionWiseEditDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public List<String> getSectionTypeCodeBasedOnTypeCode(String serviceRequestTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT sectionCode FROM VariationSectionMapping WHERE typeCode=:serviceRequestTypeCode";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<String> query = session.createQuery(hqlQuery);
		query.setParameter("serviceRequestTypeCode", serviceRequestTypeCode);
		return query.getResultList();
	}

	@Override
	public boolean checkSectionInModule(Integer moduleCode, String sectionCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		List<SectionModule> sectionModule = new ArrayList<SectionModule>();
		CriteriaQuery<SectionModule> query = builder.createQuery(SectionModule.class);
		Root<SectionModule> rootSectionModule = query.from(SectionModule.class);
		Predicate predicateOne = builder.equal(rootSectionModule.get("sectionCode"), sectionCode);
		Predicate predicateTwo = builder.equal(rootSectionModule.get("moduleCode"), moduleCode);
		query.where(builder.and(predicateOne, predicateTwo));
		sectionModule = session.createQuery(query).getResultList();
		if (sectionModule != null && !sectionModule.isEmpty()) {
			return true;
		}
		return false;
	}

	@Override
	public SectionType getSectionTypebySectionTypeId(String sectionCode) {
		return hibernateTemplate.get(SectionType.class, sectionCode);
	}

	@Override
	public ModuleVariableSection saveorUpdateModuleVariableSection(ModuleVariableSection moduleVariableSection) {
		try {
			hibernateTemplate.saveOrUpdate(moduleVariableSection);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return moduleVariableSection;
	}

	@Override
	public List<ModuleVariableSection> getEditableSections(String moduleItemKey, String subModuleItemKey, Integer moduleCode, String personId, Integer subModuleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ModuleVariableSection> query = builder.createQuery(ModuleVariableSection.class);
		Root<ModuleVariableSection> rootModuleVariableSection = query.from(ModuleVariableSection.class);
		Predicate predicateOne = builder.equal(rootModuleVariableSection.get("moduleItemKey"), moduleItemKey);
		Predicate predicateTwo = builder.equal(rootModuleVariableSection.get("moduleCode"), moduleCode);
		Predicate predicateThree = builder.equal(rootModuleVariableSection.get("personId"), personId);
		Predicate predicateFour = builder.equal(rootModuleVariableSection.get("subModuleCode"), subModuleCode);
		Predicate predicateFive = builder.equal(rootModuleVariableSection.get("subModuleItemKey"), subModuleItemKey);
		if (personId == null) {
			query.where(builder.and(predicateOne, predicateTwo, predicateFour, predicateFive));
		} else {
			query.where(builder.and(predicateOne, predicateTwo, predicateThree, predicateFour, predicateFive));
		}
		return session.createQuery(query).getResultList();	
	}

	@Override
	public List<ModuleVariableSection> getTaskEditableSections(String moduleItemKey, String subModuleItemKey, Integer moduleCode, Integer subModuleCode, String variableType, String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ModuleVariableSection> query = builder.createQuery(ModuleVariableSection.class);
		Root<ModuleVariableSection> rootModuleVariableSection = query.from(ModuleVariableSection.class);
		Predicate predicateOne = builder.equal(rootModuleVariableSection.get("moduleItemKey"), moduleItemKey);
		Predicate predicateTwo = builder.equal(rootModuleVariableSection.get("moduleCode"), moduleCode);
		Predicate predicateThree = builder.equal(rootModuleVariableSection.get("variableType"), variableType);
		Predicate predicateFour = builder.equal(rootModuleVariableSection.get("personId"), personId);
		Predicate predicateFive = builder.equal(rootModuleVariableSection.get("subModuleItemKey"), subModuleItemKey);
		Predicate predicateSix = builder.equal(rootModuleVariableSection.get("subModuleCode"), subModuleCode);
		query.where(builder.and(predicateOne, predicateTwo, predicateThree, predicateFour, predicateFive, predicateSix));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ModuleVariableSection> getModuleVariableSections(String moduleItemKey, String subModuleItemKey,
			Integer moduleCode, Integer subModuleCode, String personId, String variableType, Integer typeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builderModuleVariableSection = session.getCriteriaBuilder();
		CriteriaQuery<ModuleVariableSection> queryModuleVariableSection = builderModuleVariableSection
				.createQuery(ModuleVariableSection.class);
		Root<ModuleVariableSection> rootModuleVariableSection = queryModuleVariableSection
				.from(ModuleVariableSection.class);
		Predicate predicateModuleItemKey = builderModuleVariableSection
				.equal(rootModuleVariableSection.get("moduleItemKey"), moduleItemKey);
		Predicate predicateSubModuleItemKey = builderModuleVariableSection
				.equal(rootModuleVariableSection.get("subModuleItemKey"), subModuleItemKey);
		Predicate predicateModuleCode = builderModuleVariableSection.equal(rootModuleVariableSection.get("moduleCode"),
				moduleCode);
		Predicate predicateSubModuleCode = builderModuleVariableSection
				.equal(rootModuleVariableSection.get("subModuleCode"), subModuleCode);
		Predicate predicatePersonId = builderModuleVariableSection.equal(rootModuleVariableSection.get("personId"),
				personId);
		Predicate predicateVariableType = builderModuleVariableSection
				.equal(rootModuleVariableSection.get("variableType"), variableType);
		if (typeCode != null) {
			Predicate predicateTypeCode = builderModuleVariableSection.equal(rootModuleVariableSection.get("typeCode"),
					typeCode);
			queryModuleVariableSection.where(builderModuleVariableSection.and(predicateModuleItemKey,
					predicateSubModuleItemKey, predicateModuleCode, predicateSubModuleCode, predicatePersonId,
					predicateVariableType, predicateTypeCode));
		} else {
			queryModuleVariableSection
					.where(builderModuleVariableSection.and(predicateModuleItemKey, predicateSubModuleItemKey,
							predicateModuleCode, predicateSubModuleCode, predicatePersonId, predicateVariableType));
		}
		return session.createQuery(queryModuleVariableSection).getResultList();
	}

	@Override
	public void deleteModuleVariableSection(ModuleVariableSection moduleVariableSection) {
		try {
			hibernateTemplate.delete(moduleVariableSection);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public boolean isChangeBudgetStatus(String moduleItemKey, String subModuleItemKey, Integer moduleCode, Integer subModuleCode, String sectionTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ModuleVariableSection> query = builder.createQuery(ModuleVariableSection.class);
		Root<ModuleVariableSection> rootModuleVariableSection = query.from(ModuleVariableSection.class);
		Predicate predicateModuleItemKey = builder.equal(rootModuleVariableSection.get("moduleItemKey"), moduleItemKey);
		Predicate predicateModuleCode = builder.equal(rootModuleVariableSection.get("moduleCode"), moduleCode);
		Predicate predicateSubModuleCode = builder.equal(rootModuleVariableSection.get("subModuleCode"), subModuleCode);
		Predicate predicateSubModuleItemKey = builder.equal(rootModuleVariableSection.get("subModuleItemKey"), subModuleItemKey);
		Predicate predicateBudget = builder.equal(rootModuleVariableSection.get("sectionCode"), sectionTypeCode);
		query.where(builder.and(predicateModuleItemKey, predicateModuleCode, predicateSubModuleCode, predicateSubModuleItemKey, predicateBudget));
		return (session.createQuery(query).getResultList() != null && !session.createQuery(query).getResultList().isEmpty());
	}

	@Override
	public boolean isSectionEditableForActiveDocument(List<String> moduleItemKey, String subModuleItemKey, Integer moduleCode,
			Integer subModuleCode, String sectionTypeCode) {
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession()
				.createQuery("select count(*) from ModuleVariableSection where moduleItemKey in :moduleItemKey and moduleCode =:moduleCode and subModuleCode =:subModuleCode and subModuleItemKey =:subModuleItemKey and sectionCode =:sectionCode ");
		query.setParameter("moduleItemKey",moduleItemKey);
		query.setParameter("moduleCode",moduleCode);
		query.setParameter("subModuleCode",subModuleCode);
		query.setParameter("subModuleItemKey",subModuleItemKey);
		query.setParameter("sectionCode",sectionTypeCode);
		Integer count = Integer.valueOf(query.getSingleResult().toString());
		return count.equals(0) ? Boolean.TRUE : Boolean.FALSE;
  }
  
	public List<String> getAwardEditingSectionsBasedOnParams(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<String> outerQuery = builder.createQuery(String.class);
		Root<ModuleVariableSection> rootModuleVariableSection = outerQuery.from(ModuleVariableSection.class);
		Subquery<String> subQuery = outerQuery.subquery(String.class);
		Root<Award> rootAward = subQuery.from(Award.class);
		Predicate predicateAwardNumber = builder.equal(rootAward.get("awardNumber"), awardNumber);
		Predicate predicateAwardSequenceStatus = builder.equal(rootAward.get("awardSequenceStatus"), Constants.AWARD_FINAL_STATUS_PENDING);
		subQuery.select(rootAward.get("awardId"));
		subQuery.where(builder.and(predicateAwardNumber, predicateAwardSequenceStatus));
		Predicate predicateModuleCode = builder.equal(rootModuleVariableSection.get("moduleCode"), Constants.AWARD_MODULE_CODE);
		Predicate predicateSubModuleCode = builder.equal(rootModuleVariableSection.get("subModuleCode"), Constants.AWARD_SUBMODULE_CODE);
		outerQuery.select(rootModuleVariableSection.get("sectionCode"));
		outerQuery.where(builder.and(predicateModuleCode, predicateSubModuleCode, builder.in(rootModuleVariableSection.get("moduleItemKey")).value(subQuery)));
		return session.createQuery(outerQuery).list();
	}

	@Override
	public List<String> getEditableSectionCodes(String moduleItemKey, String subModuleItemKey, Integer moduleCode, Integer subModuleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT sectionCode FROM ModuleVariableSection WHERE moduleItemKey=:moduleItemKey and moduleCode = : moduleCode and subModuleCode = : subModuleCode and subModuleItemKey = : subModuleItemKey";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<String> query = session.createQuery(hqlQuery);
		query.setParameter("moduleItemKey", moduleItemKey);
		query.setParameter("moduleCode", moduleCode);
		query.setParameter("subModuleCode", subModuleCode);
		query.setParameter("subModuleItemKey", subModuleItemKey);
		return query.getResultList();
	}

	@Override
	public List<ModuleVariableSection> getEditableSectionsByModuleItemKeys(List<String> moduleItemKeys, String submoduleItemKey, Integer moduleCode, Integer subModuleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ModuleVariableSection> query = builder.createQuery(ModuleVariableSection.class);
		Root<ModuleVariableSection> rootModuleVariableSection = query.from(ModuleVariableSection.class);	
		Predicate predicateOne = rootModuleVariableSection.get("moduleItemKey").in(moduleItemKeys);;
		Predicate predicateTwo = builder.equal(rootModuleVariableSection.get("moduleCode"), moduleCode);
		Predicate predicateThree = builder.equal(rootModuleVariableSection.get("subModuleCode"), subModuleCode);
		Predicate predicateFour = builder.equal(rootModuleVariableSection.get("subModuleItemKey"), submoduleItemKey);
		query.where(builder.and(predicateOne, predicateTwo, predicateThree , predicateFour));
		return session.createQuery(query).getResultList();	
	}

}
