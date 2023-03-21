package com.polus.fibicomp.award.revenue.dao;

import java.sql.Timestamp;
import java.util.List;

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

import com.polus.fibicomp.award.revenue.pojo.AwardRevenueDetails;
import com.polus.fibicomp.award.revenue.pojo.AwardRevenueTransactions;

@Transactional
@Service(value = "awardRevenueDao")
public class AwardRevenueDaoImpl implements AwardRevenueDao{

	protected static Logger logger = LogManager.getLogger(AwardRevenueDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public List<AwardRevenueDetails> getRevenueDetailsByParams(String awardNumber, String accountNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardRevenueDetails> query = builder.createQuery(AwardRevenueDetails.class);
		Root<AwardRevenueDetails> parameter = query.from(AwardRevenueDetails.class);
		Predicate predicate1 = builder.equal(parameter.get("awardNumber"), awardNumber);
		Predicate predicate2 = builder.equal(parameter.get("accountNumber"), accountNumber);
		query.where(builder.and(predicate1, predicate2));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardRevenueTransactions> fetchRevenueTransactionsByParams(String awardNumber, String accountNumber, List<String> internalOrderCodes, Timestamp fiPostingStartDate, Timestamp fiPostingEndDate) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardRevenueTransactions> query = builder.createQuery(AwardRevenueTransactions.class);
		Root<AwardRevenueTransactions> parameter = query.from(AwardRevenueTransactions.class);
		Predicate predicate1 = builder.equal(parameter.get("awardNumber"), awardNumber);
		Predicate predicate2 = builder.equal(parameter.get("accountNumber"), accountNumber);
		Predicate predicate3 = parameter.get("internalOrderCode").in(internalOrderCodes);
		boolean isIOCdeNull = (internalOrderCodes != null && !internalOrderCodes.isEmpty());
		if (fiPostingStartDate != null && fiPostingEndDate != null && isIOCdeNull) {
			Predicate predicate4 = builder.between(parameter.get("fiPostingDate"), fiPostingStartDate, fiPostingEndDate);
			query.where(builder.and(predicate1, predicate2, predicate3, predicate4));
		} else if (fiPostingStartDate != null && isIOCdeNull) {
			Predicate predicate4 = builder.greaterThanOrEqualTo(parameter.get("fiPostingDate"), fiPostingStartDate);
			query.where(builder.and(predicate1, predicate2, predicate3, predicate4));
		} else if (fiPostingEndDate != null && isIOCdeNull) {
			Predicate predicate4 = builder.lessThanOrEqualTo(parameter.get("fiPostingDate"), fiPostingEndDate);
			query.where(builder.and(predicate1, predicate2, predicate3, predicate4));
		} else if (isIOCdeNull) {
			query.where(builder.and(predicate1, predicate2, predicate3));
		} else {
			query.where(builder.and(predicate1, predicate2));
		}
		query.orderBy(builder.desc(parameter.get("fiPostingDate")));
		return session.createQuery(query).getResultList();
	}

}
