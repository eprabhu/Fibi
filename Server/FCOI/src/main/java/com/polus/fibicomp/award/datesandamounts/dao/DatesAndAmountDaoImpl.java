package com.polus.fibicomp.award.datesandamounts.dao;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Subquery;

import org.hibernate.Session;
import org.hibernate.query.Query;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.award.datesandamounts.dto.DatesAndAmountsDTO;
import com.polus.fibicomp.award.datesandamounts.pojo.AwardAmountFNADistribution;
import com.polus.fibicomp.award.datesandamounts.pojo.AwardAmountTransactionHistory;
import com.polus.fibicomp.award.datesandamounts.pojo.AwardTransactionStatus;
import com.polus.fibicomp.award.pojo.AwardAmountInfo;
import com.polus.fibicomp.award.pojo.AwardAmountTransaction;
import com.polus.fibicomp.award.pojo.AwardCostShare;
import com.polus.fibicomp.award.pojo.AwardTransactionType;
import com.polus.fibicomp.award.pojo.CostShareType;
import com.polus.fibicomp.constants.Constants;

@Transactional
@Service(value = "datesAndAmountDao")
public class DatesAndAmountDaoImpl implements DatesAndAmountDao {

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public List<AwardTransactionType> getAllAwardTransactionTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardTransactionType> query = builder.createQuery(AwardTransactionType.class);
		Root<AwardTransactionType> rootAgreementSponsorType = query.from(AwardTransactionType.class);
		query.orderBy(builder.asc(rootAgreementSponsorType.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AwardAmountInfo saveOrUpdateAwardAmountInfo(AwardAmountInfo awardAmountInfo) {
		hibernateTemplate.saveOrUpdate(awardAmountInfo);
		return awardAmountInfo;
	}

	@Override
	public AwardAmountTransaction saveOrUpdateAwardAmountTransaction(AwardAmountTransaction awardAmountTransaction) {
		hibernateTemplate.saveOrUpdate(awardAmountTransaction);
		return awardAmountTransaction;
	}

	@Override
	public String getProposalNumberBasedOnProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT proposalNumber FROM InstituteProposal WHERE proposalId=:proposalId";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<String> query = session.createQuery(hqlQuery);
		query.setParameter("proposalId", proposalId);
		return query.uniqueResult();
	}

	@Override
	public List<AwardCostShare> getCostShareTypesByAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardCostShare> query = builder.createQuery(AwardCostShare.class);
		Root<AwardCostShare> costShare = query.from(AwardCostShare.class);
		query.where(builder.equal(costShare.get("awardId"), awardId));
		query.orderBy(builder.asc(costShare.get("updateTimestamp")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<String> getAwardNumbersInHierarchy(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String[] awardNumberData =awardNumber.split("-");
		final String likeCriteria = awardNumberData[0] + "%";
		@SuppressWarnings("unchecked")
		Query<String> value = session.createSQLQuery("SELECT AWARD_NUMBER FROM AWARD_HIERARCHY WHERE ROOT_AWARD_NUMBER like :likeCriteria");
		value.setParameter("likeCriteria", likeCriteria);
		return value.getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<String> getChildAwardNumbersBasedOnParentAwardNumber(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		List <String> awardNumbers = new ArrayList<String>();
		awardNumbers.add(awardNumber);
		String hqlQuery = "SELECT parentAwardNumber FROM AwardHierarchy WHERE awardNumber=:awardNumber";
		org.hibernate.query.Query<String> query = session.createQuery(hqlQuery);
		query.setParameter("awardNumber", awardNumber);
		String parentAwardNumber = query.uniqueResult();
		while (!parentAwardNumber.equals("000000-00000")) {
			awardNumbers.add(parentAwardNumber);
			String hqlQueryTwo = "SELECT parentAwardNumber FROM AwardHierarchy WHERE awardNumber=:awardNumber";
			org.hibernate.query.Query<String> queryTwo = session.createQuery(hqlQueryTwo);
			queryTwo.setParameter("awardNumber", parentAwardNumber);
			parentAwardNumber = queryTwo.uniqueResult();
		}
		return awardNumbers;
	}

	@SuppressWarnings("unchecked")
	@Override
	public DatesAndAmountsDTO getAllIntermediateChildAwards(String sourceAwardNumber, String destinationAwardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		DatesAndAmountsDTO datesAndAmountsDTO = new DatesAndAmountsDTO();
		List <String> awardNumbers = new ArrayList<String>();
		boolean isBreak = false;
		awardNumbers.add(destinationAwardNumber);
		String getParentAwardNumber = "SELECT parentAwardNumber FROM AwardHierarchy WHERE awardNumber=:awardNumber";
		org.hibernate.query.Query<String> getParentAwardNumberQuery = session.createQuery(getParentAwardNumber);
		getParentAwardNumberQuery.setParameter("awardNumber", destinationAwardNumber);
		String parentAwardNumber = getParentAwardNumberQuery.uniqueResult();
		while (!parentAwardNumber.equals(sourceAwardNumber)) {
			awardNumbers.add(parentAwardNumber);
			if (parentAwardNumber.equals("000000-00000")) {
				isBreak = true;
				datesAndAmountsDTO.setCanAddTotal(true);
				break;
			}
			String getParentAwardNumbers = "SELECT parentAwardNumber FROM AwardHierarchy WHERE awardNumber=:awardNumber";
			org.hibernate.query.Query<String> getParentAwardNumbersQuery = session.createQuery(getParentAwardNumbers);
			getParentAwardNumbersQuery.setParameter("awardNumber", parentAwardNumber);
			parentAwardNumber = getParentAwardNumbersQuery.uniqueResult();
		}
		if (isBreak) {
			awardNumbers.clear();
			awardNumbers.add(destinationAwardNumber);
			String getDestParentAwardNumber = "SELECT parentAwardNumber FROM AwardHierarchy WHERE awardNumber=:awardNumber";
			org.hibernate.query.Query<String> getDestParentAwardNumberQuery = session.createQuery(getDestParentAwardNumber);
			getDestParentAwardNumberQuery.setParameter("awardNumber", sourceAwardNumber);
			parentAwardNumber = getDestParentAwardNumberQuery.uniqueResult();
			while (!parentAwardNumber.equals(destinationAwardNumber)) {
				awardNumbers.add(parentAwardNumber);
				if (parentAwardNumber.equals("000000-00000")) {
					isBreak = true;
					datesAndAmountsDTO.setUnRelatedTransaction(true);
					break;
				}
				String getDestParentAwardNumbers = "SELECT parentAwardNumber FROM AwardHierarchy WHERE awardNumber=:awardNumber";
				org.hibernate.query.Query<String> getDestParentAwardNumbersQuery = session.createQuery(getDestParentAwardNumbers);
				getDestParentAwardNumbersQuery.setParameter("awardNumber", parentAwardNumber);
				parentAwardNumber = getDestParentAwardNumbersQuery.uniqueResult();
			}
		}
		datesAndAmountsDTO.setAwardNumbers(awardNumbers);
		return datesAndAmountsDTO;
	}

	@Override
	public List<CostShareType> getBudgetIncludedCostshareType(Boolean canIncludeInBudget) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CostShareType> query = builder.createQuery(CostShareType.class);
		Root<CostShareType> costShareType = query.from(CostShareType.class);
		query.where(builder.equal(costShareType.get("canIncludeInBudget"), canIncludeInBudget));
		query.orderBy(builder.asc(costShareType.get("updateTimestamp")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void deleteAwardAmountTransaction(AwardAmountTransaction awardAmountTransaction) {
		hibernateTemplate.delete(awardAmountTransaction);
	}

	@Override
	public AwardTransactionStatus fetchAwardTransactionStatusById(String transactionStatusCode) {
		return hibernateTemplate.get(AwardTransactionStatus.class, transactionStatusCode);
	}

	@Override
	public void updateDatesAndAmounts(Integer awardId, String awardNumber, String activeTransaction) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaUpdate<AwardAmountTransaction> query = builder.createCriteriaUpdate(AwardAmountTransaction.class);
		Root<AwardAmountTransaction> root = query.from(AwardAmountTransaction.class);
		Predicate predicateOne = builder.equal(root.get("awardNumber"), awardNumber);
		Predicate predicateTwo = builder.equal(root.get("transactionStatusCode"), Constants.PENDING_TRANSACTION);
		query.set(root.get("transactionStatusCode"), activeTransaction).where(builder.and(predicateOne, predicateTwo));
		session.createQuery(query).executeUpdate();
	}

	@Override
	public AwardAmountTransactionHistory saveOrUpdateAwardAmountTransactionHistory(AwardAmountTransactionHistory awardAmountTransactionHistory) {
		hibernateTemplate.saveOrUpdate(awardAmountTransactionHistory);
		return awardAmountTransactionHistory;
	}

	@Override
	public BigDecimal fetchLatestTransactionCode(Integer awardId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<BigDecimal> query = builder.createQuery(BigDecimal.class);
			Root<AwardAmountTransactionHistory> root = query.from(AwardAmountTransactionHistory.class);
			query.select(root.get("transactionId"));
			query.where(builder.equal(root.get("awardId"), awardId));
			return  session.createQuery(query).uniqueResult();
		} catch (Exception e) {
			throw new ApplicationException("Error occurred in fetchLatestTransactionCode", e, Constants.JAVA_ERROR);
		}
	}

	@Override
	public List<AwardAmountInfo> getAwardAmountInfoBasedOnParams(String awardNumber, BigDecimal transactionId, Integer awardSequenceNumber, String awardSequenceStatus) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardAmountInfo> query = builder.createQuery(AwardAmountInfo.class);
		Root<AwardAmountInfo> rootAwardAmountInfo = query.from(AwardAmountInfo.class);
		Predicate predicateOne = builder.equal(rootAwardAmountInfo.get("awardNumber"), awardNumber);
		if (awardSequenceNumber == 0) {
			Predicate predicateThree = rootAwardAmountInfo.get("awardAmountTransaction").get("transactionStatusCode").in(Constants.ACTIVE_TRANSACTION, Constants.PENDING_TRANSACTION);
			query.where(builder.and(predicateOne, predicateThree));
			return session.createQuery(query).getResultList();
		} else if (awardSequenceStatus.equals(Constants.AWARD_FINAL_STATUS_PENDING)) {
			query.where(builder.and(predicateOne));
		} else {
			if (transactionId != null) {
				Subquery<Integer> subQuery = query.subquery(Integer.class);
				Root<AwardAmountInfo> subRootAwardAmountInfo = subQuery.from(AwardAmountInfo.class);
				Predicate predicateTransactionId = builder.equal(subRootAwardAmountInfo.get("transactionId"), transactionId);
				subQuery.select(builder.max(subRootAwardAmountInfo.get("awardAmountInfoId")))
						.where(builder.and(predicateOne, predicateTransactionId));
				Predicate predicateThree = builder.lessThanOrEqualTo(rootAwardAmountInfo.get("awardAmountInfoId"),
						subQuery);
				query.where(builder.and(predicateOne, predicateThree));
			} else {
				return new ArrayList<>();
			}
		}
		List<AwardAmountInfo> awardAmountInfos = session.createQuery(query).getResultList();
		List<AwardAmountInfo> filteredAwardAmountInfos = new ArrayList<>();
		List<Integer> awardSequenceNumbers = new ArrayList<>();
		awardSequenceNumbers.add(awardSequenceNumber);
		awardAmountInfos.stream().filter((awardAmountInfo -> awardAmountInfo.getAwardAmountTransaction() != null)).
		filter(awardAmountInfo -> awardAmountInfo.getAwardAmountTransaction().getTransactionStatusCode() != null)
				.filter(awardAmountInfo -> ((awardAmountInfo.getAwardAmountTransaction().getTransactionStatusCode()
						.equals(Constants.CANCELLED_TRANSACTION)
						&& awardSequenceNumbers.contains(awardAmountInfo.getSequenceNumber())
						&& awardAmountInfo.getAwardAmountTransaction().getAwardNumber().equals(awardNumber))
						|| (!awardAmountInfo.getAwardAmountTransaction().getTransactionStatusCode()
								.equals(Constants.CANCELLED_TRANSACTION))))
				.forEach(awardAmountInfo -> {
					filteredAwardAmountInfos.add(awardAmountInfo);
				});
		return filteredAwardAmountInfos;
	}

	@Override
	public AwardAmountInfo getLatestAwardAmountInfo(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardAmountInfo> outerQuery = builder.createQuery(AwardAmountInfo.class);
		Root<AwardAmountInfo> rootAwardAmountInfo = outerQuery.from(AwardAmountInfo.class);
		Subquery<Integer> subQuery = outerQuery.subquery(Integer.class);
		Root<AwardAmountInfo> subRootAwardAmountInfo = subQuery.from(AwardAmountInfo.class);
		Predicate predicateAwardNumber = builder.equal(subRootAwardAmountInfo.get("awardNumber"), awardNumber);
		Predicate predicateTransactionStatus = builder.notEqual(
				subRootAwardAmountInfo.get("awardAmountTransaction").get("transactionStatusCode"),
				Constants.CANCELLED_TRANSACTION);
		subQuery.select(builder.max(subRootAwardAmountInfo.get("awardAmountInfoId")))
				.where(builder.and(predicateAwardNumber, predicateTransactionStatus));
		outerQuery.where(builder.in(rootAwardAmountInfo.get("awardAmountInfoId")).value(subQuery));
		return session.createQuery(outerQuery).uniqueResult();
	}

	@Override
	public AwardAmountInfo getLatestActiveAwardAmountInfo(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardAmountInfo> outerQuery = builder.createQuery(AwardAmountInfo.class);
		Root<AwardAmountInfo> rootAwardAmountInfo = outerQuery.from(AwardAmountInfo.class);
		Subquery<Integer> subQuery = outerQuery.subquery(Integer.class);
		Root<AwardAmountInfo> subRootAwardAmountInfo = subQuery.from(AwardAmountInfo.class);
		Predicate predicateAwardNumber = builder.equal(subRootAwardAmountInfo.get("awardNumber"), awardNumber);
		Predicate predicateTransactionStatus = builder.equal(
				subRootAwardAmountInfo.get("awardAmountTransaction").get("transactionStatusCode"),
				Constants.ACTIVE_TRANSACTION);
		subQuery.select(builder.max(subRootAwardAmountInfo.get("awardAmountInfoId")))
				.where(builder.and(predicateAwardNumber, predicateTransactionStatus));
		outerQuery.where(builder.in(rootAwardAmountInfo.get("awardAmountInfoId")).value(subQuery));
		return session.createQuery(outerQuery).uniqueResult();
	}

	@Override
	public AwardAmountInfo getLatestPendingAwardAmountInfo(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardAmountInfo> outerQuery = builder.createQuery(AwardAmountInfo.class);
		Root<AwardAmountInfo> rootAwardAmountInfo = outerQuery.from(AwardAmountInfo.class);
		Subquery<Integer> subQuery = outerQuery.subquery(Integer.class);
		Root<AwardAmountInfo> subRootAwardAmountInfo = subQuery.from(AwardAmountInfo.class);
		Predicate predicateAwardNumber = builder.equal(subRootAwardAmountInfo.get("awardNumber"), awardNumber);
		Predicate predicateTransactionStatus = builder.equal(subRootAwardAmountInfo.get("awardAmountTransaction").get("transactionStatusCode"),
				Constants.PENDING_TRANSACTION);
		subQuery.select(builder.max(subRootAwardAmountInfo.get("awardAmountInfoId"))).where(builder.and(predicateAwardNumber, predicateTransactionStatus));
		outerQuery.where(builder.in(rootAwardAmountInfo.get("awardAmountInfoId")).value(subQuery));
		return session.createQuery(outerQuery).uniqueResult();
	}

	@Override
	public AwardAmountInfo getUsableAwardAmountInfo(String awardNumber, String transactionStatus) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardAmountInfo> outerQuery = builder.createQuery(AwardAmountInfo.class);
		Root<AwardAmountInfo> rootAwardAmountInfo = outerQuery.from(AwardAmountInfo.class);
		Subquery<Integer> subQuery = outerQuery.subquery(Integer.class);
		Root<AwardAmountInfo> subRootAwardAmountInfo = subQuery.from(AwardAmountInfo.class);
		Predicate predicateAwardNumber = builder.equal(subRootAwardAmountInfo.get("awardNumber"), awardNumber);
		Predicate predicateTransactionStatus = builder.equal(subRootAwardAmountInfo.get("awardAmountTransaction").get("transactionStatusCode"),
				Constants.ACTIVE_TRANSACTION);
		Predicate subAwardNumber = builder.equal(subRootAwardAmountInfo.get("awardAmountTransaction").get("awardNumber"),
				awardNumber);
		Predicate predicateStatus = null;
		if (transactionStatus.equals(Constants.CANCELLED_TRANSACTION)) {
			predicateStatus = builder.equal(subRootAwardAmountInfo.get("awardAmountTransaction").get("transactionStatusCode"),
					Constants.CANCELLED_TRANSACTION);
		} else {
			predicateStatus = builder.equal(subRootAwardAmountInfo.get("awardAmountTransaction").get("transactionStatusCode"),
					Constants.ACTIVE_TRANSACTION);
		}
		Predicate andPredicateOne = builder.and(predicateAwardNumber, predicateTransactionStatus);
		Predicate andPredicateTwo = builder.and(subAwardNumber, predicateAwardNumber, predicateStatus);
		Predicate orPredicate = builder.or(andPredicateOne, andPredicateTwo);
		subQuery.select(builder.max(subRootAwardAmountInfo.get("awardAmountInfoId"))).where(orPredicate);
		outerQuery.where(builder.in(rootAwardAmountInfo.get("awardAmountInfoId")).value(subQuery));
		return session.createQuery(outerQuery).uniqueResult();
	}

	@Override
	public void deleteTransactionsBasedOnTransactionId(BigDecimal transactionId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<AwardAmountInfo> delete = builder.createCriteriaDelete(AwardAmountInfo.class);
		Root<AwardAmountInfo> root = delete.from(AwardAmountInfo.class);
		delete.where(builder.equal(root.get("transactionId"), transactionId));
		session.createQuery(delete).executeUpdate();
	}

	@Override
	public void deleteAwardAmountTransaction(BigDecimal transactionId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<AwardAmountTransaction> delete = builder.createCriteriaDelete(AwardAmountTransaction.class);
		Root<AwardAmountTransaction> root = delete.from(AwardAmountTransaction.class);
		delete.where(builder.equal(root.get("transactionId"), transactionId));
		session.createQuery(delete).executeUpdate();
	}

	@Override
	public AwardAmountTransaction getIsDatesAndAmountsEditable(String splitedAwardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardAmountTransaction> outerQuery = builder.createQuery(AwardAmountTransaction.class);
		Root<AwardAmountTransaction> rootAwardAmountTransaction = outerQuery.from(AwardAmountTransaction.class);
		Subquery<Integer> subQuery = outerQuery.subquery(Integer.class);
		Root<AwardAmountTransaction> subRootAwardAmountTransaction = subQuery.from(AwardAmountTransaction.class);
		Predicate predicateAwardNumber = builder.like(subRootAwardAmountTransaction.get("awardNumber"), splitedAwardNumber + "%");
		Predicate predicateTransactionStatus = builder.equal(subRootAwardAmountTransaction.get("transactionStatusCode"), Constants.PENDING_TRANSACTION);
		subQuery.select(builder.max(subRootAwardAmountTransaction.get("awardAmountTransactionId"))).where(builder.and(predicateAwardNumber, predicateTransactionStatus));
		outerQuery.where(builder.in(rootAwardAmountTransaction.get("awardAmountTransactionId")).value(subQuery));
		return session.createQuery(outerQuery).uniqueResult();
	}

	@Override
	public AwardAmountInfo fetchLastAwardAmountInfo(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardAmountInfo> outerQuery = builder.createQuery(AwardAmountInfo.class);
		Root<AwardAmountInfo> rootAwardAmountInfo = outerQuery.from(AwardAmountInfo.class);
		Subquery<Integer> subQuery = outerQuery.subquery(Integer.class);
		Root<AwardAmountInfo> subRootAwardAmountInfo = subQuery.from(AwardAmountInfo.class);
		Predicate predicateAwardNumber = builder.equal(subRootAwardAmountInfo.get("awardNumber"), awardNumber);
		subQuery.select(builder.max(subRootAwardAmountInfo.get("awardAmountInfoId")))
				.where(builder.and(predicateAwardNumber));
		outerQuery.where(builder.in(rootAwardAmountInfo.get("awardAmountInfoId")).value(subQuery));
		return session.createQuery(outerQuery).uniqueResult();
	}

	@Override
	public AwardAmountInfo getAwardAmountInfoBasedOnAwardDetail(Integer awardAmountInfoId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardAmountInfo> outerQuery = builder.createQuery(AwardAmountInfo.class);
		Root<AwardAmountInfo> rootAwardAmountInfo = outerQuery.from(AwardAmountInfo.class);
		outerQuery.where(builder.equal(rootAwardAmountInfo.get("awardAmountInfoId"), awardAmountInfoId));
		return session.createQuery(outerQuery).uniqueResult();
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public List<AwardAmountInfo> getAllUnrelatedTransactions(List<BigDecimal> transactionIds, String awardNumber) {
		try {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("select a1 from AwardAmountInfo a1 left join AwardAmountTransaction a2");
		hqlQuery.append(" on a1.transactionId = a2.transactionId and a1.awardNumber = a2.destinationAwardNumber");
		hqlQuery.append(" where a2.awardNumber =:awardNumber ");
		hqlQuery.append(" and a2.transactionStatusCode ='P'");
		if (!transactionIds.isEmpty()) {
			hqlQuery.append(" and a2.transactionId NOT IN(:transactionId)");
		}
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		query.setParameter("awardNumber", awardNumber);
		if (!transactionIds.isEmpty()) {
			query.setParameter("transactionId", transactionIds);
		}
		return  query.getResultList();
		} catch(Exception e) {
			return new ArrayList<>();
		}
	}

	@Override
	public AwardAmountFNADistribution saveOrUpdateAwardAmountFNADistribution(AwardAmountFNADistribution awardAmountFNADistribution) {
		hibernateTemplate.saveOrUpdate(awardAmountFNADistribution);
		return awardAmountFNADistribution;
	}

	@Override
	public Integer getLatestAwardNumber(String awardNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("SELECT max(t1.awardId) FROM Award t1 inner join ModuleVariableSection t2 on t1.awardId = t2.moduleItemKey ");
			hqlQuery.append("WHERE t1.awardNumber=:awardNumber and t1.awardSequenceStatus = 'PENDING'");
			hqlQuery.append("and t2.sectionCode ='108'");
			javax.persistence.Query query = session.createQuery(hqlQuery.toString());
			query.setParameter("awardNumber", awardNumber);
			return (Integer) query.getSingleResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public List<AwardAmountFNADistribution> getAwardAmountFNADistributionBasedOnAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardAmountFNADistribution> query = builder.createQuery(AwardAmountFNADistribution.class);
		Root<AwardAmountFNADistribution> rootFnaDistribution = query.from(AwardAmountFNADistribution.class);
		query.where(builder.equal(rootFnaDistribution.get("awardId"), awardId));
		return session.createQuery(query).getResultList();
	}

	
	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public List<AwardAmountFNADistribution> getAwardAmountFNADistributionBasedOnParam(String awardNumber, Integer sequenceNumber, Integer awardId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("from AwardAmountFNADistribution where awardId in (select max(t1.awardId) from AwardAmountFNADistribution");
		hqlQuery.append(" t1 inner join Award t2 on t1.awardId=t2.awardId");
		hqlQuery.append(" where t1.awardNumber =:awardNumber and t1.sequenceNumber <= :sequenceNumber and t2.awardSequenceStatus = 'ARCHIVE')");
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		query.setParameter("awardNumber", awardNumber);
		query.setParameter("sequenceNumber", sequenceNumber);
		return query.getResultList();
	}

	@Override
	public Integer getLatestArchiveTransactionAwardBasedOnParam(Integer awardId, String awardNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("select max(awardId) from AwardAmountInfo where transactionId in ( ");
			hqlQuery.append("select transactionId from AwardAmountTransactionHistory where awardId = :awardId) and awardNumber =:awardNumber");
			javax.persistence.Query query = session.createQuery(hqlQuery.toString());
			query.setParameter("awardId", awardId);
			query.setParameter("awardNumber", awardNumber);
			return (Integer) query.getSingleResult();
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	@Override
	public void deleteAwardAmountFNADistribution(AwardAmountFNADistribution awardAmountFNADistribution) {
		hibernateTemplate.delete(awardAmountFNADistribution);
	}

	@Override
	public AwardAmountFNADistribution fetchAwardAmountFNADistributionById(Integer fnaDistributionId) {
		return hibernateTemplate.get(AwardAmountFNADistribution.class, fnaDistributionId);
	}
} 
