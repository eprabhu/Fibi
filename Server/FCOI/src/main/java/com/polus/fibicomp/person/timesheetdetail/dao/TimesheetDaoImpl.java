package com.polus.fibicomp.person.timesheetdetail.dao;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardKeyPersonTimesheet;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.person.timesheetdetail.vo.TimesheetVO;

@Transactional
@Service(value = "timesheetDao")
public class TimesheetDaoImpl implements TimesheetDao {

	protected static Logger logger = LogManager.getLogger(TimesheetDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public List<Award> getAwardTimesheetByPersonId(TimesheetVO vo) {
		try {
			StringBuilder titleLikeCriteria = new StringBuilder();
			StringBuilder awardNumberLikeCriteria = new StringBuilder();
			StringBuilder sponsorAwardNumberLikeCriteria = new StringBuilder();
			if (vo.getProperty3() != null) {
				titleLikeCriteria = titleLikeCriteria.append("%").append(vo.getProperty3()).append("%");
			}
			if (vo.getProperty1() != null) {
				awardNumberLikeCriteria = awardNumberLikeCriteria.append("%").append(vo.getProperty1()).append("%");
			}
			if (vo.getProperty2() != null) {
				sponsorAwardNumberLikeCriteria = sponsorAwardNumberLikeCriteria.append("%").append(vo.getProperty2()).append("%");
			}
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Award> query = builder.createQuery(Award.class);
			Root<Award> award= query.from(Award.class);
			Predicate predicateSequenceStatus = builder.equal(award.get("awardSequenceStatus"), "ACTIVE");
			Join<Award, AwardPerson> join = award.join("awardPersons");
			List<Predicate> inRestrictions = new ArrayList<>();
			Predicate predicatePersonId= builder.equal(join.get("personId"), vo.getPersonId());
			Predicate predicatePersonRole = join.get("personRoleId").in(Constants.COI_ROLE_CODE, Constants.PI_ROLE_CODE);
			Predicate predicateTitle = builder.like(award.get("title"), titleLikeCriteria.toString());
			Predicate predicateAwardNumber = builder.like(award.get("awardNumber"), awardNumberLikeCriteria.toString());
			Predicate predicateSponsorAwardNumber = builder.like(award.get("sponsorAwardNumber"), sponsorAwardNumberLikeCriteria.toString());
			Predicate predicateLeadUnitNumber = builder.equal(award.get("leadUnitNumber"), vo.getProperty4());
			Predicate predicateSponsorCode = builder.equal(award.get("sponsorCode"), vo.getProperty5());
			Predicate predicateAwardTypeCodes = (builder.in(award.get("awardTypeCode")).value(vo.getProperty6()));
			if (vo.getProperty1() != null && !vo.getProperty1().isEmpty()) {
				inRestrictions.add(predicateAwardNumber);
			}
			if(vo.getProperty2() != null && !vo.getProperty2().isEmpty()) {
				inRestrictions.add(predicateSponsorAwardNumber);
			}
			if (vo.getProperty3() != null && !vo.getProperty3().isEmpty()) {
				inRestrictions.add(predicateTitle);
			}
			if(vo.getProperty4() != null && !vo.getProperty4().isEmpty()) {
				inRestrictions.add(predicateLeadUnitNumber);
			}
			if (vo.getProperty5() != null && !vo.getProperty5().isEmpty()) {
				inRestrictions.add(predicateSponsorCode);
			}
			if(!vo.getProperty6().isEmpty() && vo.getProperty6() != null) {
				inRestrictions.add(predicateAwardTypeCodes);
			}
			inRestrictions.add(predicateSequenceStatus);
			inRestrictions.add(predicatePersonId);
			inRestrictions.add(predicatePersonRole);
			query.where(builder.and(inRestrictions.toArray(new Predicate[inRestrictions.size()])));
			if (vo.getReverse() != null && !vo.getReverse().isEmpty()) {
				if (vo.getReverse().equals("ASC")) {
					if (vo.getSortBy().equals("awardNumber")) {
						query.orderBy(builder.asc(award.get("awardNumber")));
					} else if (vo.getSortBy().equals("sponsorAwardNumber")) {
						query.orderBy(builder.asc(award.get("sponsorAwardNumber")));
					} else if (vo.getSortBy().equals("title")) {
						query.orderBy(builder.asc(award.get("title")));
					} else if (vo.getSortBy().equals("leadUnit")) {
						query.orderBy(builder.asc(award.get("leadUnit").get("unitName")));
					} 
					else if (vo.getSortBy().equals("role")) {
						query.orderBy(builder.asc(join.get("proposalPersonRole").get("description")));
					}
					else if (vo.getSortBy().equals("principalInvestigator")) {
						query.orderBy(builder.asc(join.get("fullName")));
					}
					else if (vo.getSortBy().equals("sponsor")) {
						query.orderBy(builder.asc(award.get("sponsor").get("sponsorName")));
					} else if (vo.getSortBy().equals("beginDate")) {
						query.orderBy(builder.asc(award.get("beginDate")));
					} else if (vo.getSortBy().equals("finalExpirationDate")) {
						query.orderBy(builder.asc(award.get("finalExpirationDate")));
					} else if (vo.getSortBy().equals("awardType")) {
						query.orderBy(builder.asc(award.get("awardType").get("description")));
					}
				} else {
					if (vo.getSortBy().equals("awardNumber")) {
						query.orderBy(builder.desc(award.get("awardNumber")));
					} else if (vo.getSortBy().equals("sponsorAwardNumber")) {
						query.orderBy(builder.desc(award.get("sponsorAwardNumber")));
					} else if (vo.getSortBy().equals("title")) {
						query.orderBy(builder.desc(award.get("title")));
					} else if (vo.getSortBy().equals("leadUnit")) {
						query.orderBy(builder.desc(award.get("leadUnit").get("unitName")));
					} else if (vo.getSortBy().equals("role")) {
						query.orderBy(builder.desc(join.get("proposalPersonRole").get("description")));
					} else if (vo.getSortBy().equals("principalInvestigator")) {
						query.orderBy(builder.desc(join.get("fullName")));
					} else if (vo.getSortBy().equals("sponsor")) {
						query.orderBy(builder.desc(award.get("sponsor").get("sponsorName")));
					} else if (vo.getSortBy().equals("beginDate")) {
						query.orderBy(builder.desc(award.get("beginDate")));
					} else if (vo.getSortBy().equals("finalExpirationDate")) {
						query.orderBy(builder.desc(award.get("finalExpirationDate")));
					} else if (vo.getSortBy().equals("awardType")) {
						query.orderBy(builder.desc(award.get("awardType").get("description")));
					}
				}
			} else {
				query.orderBy(builder.desc(award.get("createTimestamp")));
			}
			Integer count = (session.createQuery(query).getResultList()).size();
			vo.setTotalRecords(count);
			return session.createQuery(query).setFirstResult((vo.getCurrentPage() - 1) * vo.getItemsPerPage()).setMaxResults(vo.getItemsPerPage()).getResultList();
			} catch (Exception e) {
			logger.error("Error in getAwardTimesheetByPersonId {}", e.getMessage());
			return Collections.emptyList();          
		}	
	}

	@Override
	public AwardKeyPersonTimesheet saveOrUpdateAwardKeyPersonTimesheet(AwardKeyPersonTimesheet awardKeyPersonTimesheet) {
		try {
			hibernateTemplate.saveOrUpdate(awardKeyPersonTimesheet);
		} catch (Exception e) {
			logger.info("Error ocuured in saveOrUpdateAwardKeyPersonTimesheet {}", e.getMessage());
		}
		return awardKeyPersonTimesheet;
	}

	@Override
	public List<AwardKeyPersonTimesheet> getAwardKeyPersonTimesheetByParams(Integer awardPersonId, Integer awardId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AwardKeyPersonTimesheet> query = builder.createQuery(AwardKeyPersonTimesheet.class);
			Root<AwardKeyPersonTimesheet> rootAwardKeyPersonTimesheet = query.from(AwardKeyPersonTimesheet.class);
			Predicate predicateAwardPersonId = builder.equal(rootAwardKeyPersonTimesheet.get("awardPersonId"), awardPersonId);
			Predicate predicateAwardId = builder.equal(rootAwardKeyPersonTimesheet.get("awardId"), awardId);
			query.where(builder.and(predicateAwardPersonId, predicateAwardId));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			logger.error("Error in getAwardKeyPersonTimesheetByParams {}", e.getMessage());
			return new ArrayList<>();
		}
		
	}

	@Override
	public void deleteAwardKeyPersonTimesheet(AwardKeyPersonTimesheet awardKeyPersonTimesheet) {
		hibernateTemplate.delete(awardKeyPersonTimesheet);
	}

	@Override
	public String getAwardKeyPersonTimesheetType(Integer awardId) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append(" SELECT timesheetType FROM AwardKeyPersonTimesheet WHERE");
			hqlQuery.append("  keypersonTimesheetId = (select max(keypersonTimesheetId) FROM AwardKeyPersonTimesheet where awardId=:awardId)");
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			query.setParameter("awardId", awardId);
			return (String) query.getSingleResult();
		} catch (Exception e) {
			logger.error("error in getAwardKeyPersonTimesheetType {}", e.getMessage());
			return null;
		}
	}

	@Override
	public void deleteAwardKeyPersonTimesheetByParams(Integer awardPersonId, Integer awardId) {
		try {
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
					"delete from AwardKeyPersonTimesheet where awardPersonId = :awardPersonId AND awardId = :awardId");
			query.setParameter("awardPersonId", awardPersonId);
			query.setParameter("awardId", awardId);
			query.executeUpdate();
		} catch (Exception e) {
			logger.error("Exception in deleteAwardKeyPersonTimesheetByParams : {}", e.getMessage());
		}
	}

}
