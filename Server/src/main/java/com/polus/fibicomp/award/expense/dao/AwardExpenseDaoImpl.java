package com.polus.fibicomp.award.expense.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.hibernate.query.Query;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.expense.pojo.AwardExpenseDetail;
import com.polus.fibicomp.award.expense.pojo.AwardExpenseDetailsExt;
import com.polus.fibicomp.award.expense.pojo.AwardExpenseHeader;
import com.polus.fibicomp.award.expense.pojo.AwardExpenseTransaction;
import com.polus.fibicomp.award.expense.pojo.AwardHoursLogged;
import com.polus.fibicomp.award.expense.vo.AwardExpenseTransactionVO;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "awardExpenseDao")
public class AwardExpenseDaoImpl implements AwardExpenseDao {

	protected static Logger logger = LogManager.getLogger(AwardExpenseDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Value("${oracledb}")
	private String oracledb;

	@Override
	public ResultSet loadExpenseDetailsByAwardId(String awardNumber, String accountNumber, String type) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet rset = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				if (type.equals("E")) {
					statement = connection.prepareCall("{call get_award_expense_tracking(?,?)}");
				} else {
					statement = connection.prepareCall("{call get_award_purchase_details(?,?)}");
				}
				statement.setString(1, awardNumber);
				statement.setString(2, accountNumber);
				statement.execute();
				rset = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				if (type.equals("E")) {
					statement = connection.prepareCall("{call get_award_expense_tracking(?,?,?)}");
				} else {
					statement = connection.prepareCall("{call get_award_purchase_details(?,?,?)}");
				}
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, awardNumber);
				statement.setString(3, accountNumber);
				statement.execute();
				rset = (ResultSet) statement.getObject(1);
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return rset;
	}

	@SuppressWarnings({ "unchecked" })
	@Override
	public List<AwardExpenseTransactionVO> fetchAwardExpenseTransactionsByParams(String awardNumber,String accountNumber, String internalOrderCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String query = "SELECT FM_POSTING_DATE, BANK_CLEARING_DATE, DOCUMENT_DATE, FI_GL_ACCOUNT, FI_GL_DESCRIPTION, AMOUNT_IN_FMA_CURRENCY, VENDOR_CODE, VENDOR_NAME, ASSET_LOCATION, REMARKS FROM AWARD_EXPENSE_TRANSACTIONS WHERE AWARD_NUMBER=:awardNumber AND ACCOUNT_NUMBER=:accountNumber AND INTERNAL_ORDER_CODE=:internalOrderCode";
		Query<Object[]> sqlQuery = session.createSQLQuery(query);
		sqlQuery.setParameter("awardNumber", awardNumber);
		sqlQuery.setParameter("accountNumber", accountNumber);
		sqlQuery.setParameter("internalOrderCode", internalOrderCode);
		List<Object[]> entities = sqlQuery.list();
		List<AwardExpenseTransactionVO> awardExpenseTransactionVOs = new ArrayList<>();
		for (Object[] entity : entities) {
			AwardExpenseTransactionVO awardExpenseTransactionVO = new AwardExpenseTransactionVO();
			awardExpenseTransactionVO.setFiPostingDate(entity[0].toString());
			awardExpenseTransactionVO.setBankClearingDate(entity[1].toString());
			awardExpenseTransactionVO.setDocumentDate(entity[2].toString());
			awardExpenseTransactionVO.setFiGlAccount(entity[3].toString());
			awardExpenseTransactionVO.setFiGlDescription(entity[4].toString());
			awardExpenseTransactionVO.setAmountInFmaCurrency(entity[5].toString());
			awardExpenseTransactionVO.setVendorCode(entity[6].toString());
			awardExpenseTransactionVO.setVendorName(entity[7].toString());
			awardExpenseTransactionVO.setAssetLocation(entity[8].toString());
			awardExpenseTransactionVO.setRemarks(entity[9].toString());
			awardExpenseTransactionVOs.add(awardExpenseTransactionVO);
		}
		return awardExpenseTransactionVOs;
	}

	@Override
	public List<AwardHoursLogged> fetchPersonLogDetailsByParams(String awardNumber, String accountNumber, String internalOrderCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardHoursLogged> query = builder.createQuery(AwardHoursLogged.class);
		Root<AwardHoursLogged> rootAwardHoursLogged = query.from(AwardHoursLogged.class);
		Predicate predicateOne = builder.equal(rootAwardHoursLogged.get("awardNumber"),awardNumber);
		Predicate predicateTwo = builder.equal(rootAwardHoursLogged.get("accountNumber"),accountNumber);
		Predicate predicateThree = builder.equal(rootAwardHoursLogged.get("internalOrderCode"),internalOrderCode);
		query.where(builder.and(predicateOne, predicateTwo, predicateThree));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AwardExpenseDetailsExt updateCommittedAmount(AwardExpenseDetailsExt awardExpenseDetailsExt) {
		try {
			hibernateTemplate.saveOrUpdate(awardExpenseDetailsExt);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return awardExpenseDetailsExt;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<AwardExpenseDetailsExt> fetchAwardExpenseDetailsExtByParams(String awardNumber, String accountNumber, String internalOrderCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "FROM AwardExpenseDetailsExt WHERE awardNumber=:awardNumber and accountNumber=:accountNumber "
				+ "and internalOrderCode=:internalOrderCode";
		Query<AwardExpenseDetailsExt> query = session.createQuery(hqlQuery);
		query.setParameter("awardNumber", awardNumber);
		query.setParameter("accountNumber", accountNumber);
		query.setParameter("internalOrderCode", internalOrderCode);
		return query.getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<AwardExpenseTransaction> fetchExpenseTransactionsDetailsByParams(String awardNumber, String accountNumber, String internalOrderCode, String actualOrCommittedFlag) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String ioCode = internalOrderCode == null ? "is null" : "= '" + internalOrderCode + "'";
		String expenseTransactionQuery = "FROM AwardExpenseTransaction WHERE awardNumber= :awardNumber and accountNumber=:accountNumber and internalOrderCode = :ioCode and actualOrCommittedFlag= :actualOrCommittedFlag";
		Query<AwardExpenseTransaction> query = session.createQuery(expenseTransactionQuery);
		query.setParameter("awardNumber", awardNumber);
		query.setParameter("accountNumber", accountNumber);
		query.setParameter("ioCode", ioCode);
		query.setParameter("actualOrCommittedFlag", actualOrCommittedFlag);
		return query.getResultList();
	}

	@Override
	public AwardExpenseDetail fetchAwardExpenseDetailByParams(String awardNumber, String accountNumber, String internalOrderCode) {
		List<AwardExpenseDetail> awardExpenseDetails = null;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardExpenseDetail> query = builder.createQuery(AwardExpenseDetail.class);
		Root<AwardExpenseDetail> rootAwardExpenseDetail = query.from(AwardExpenseDetail.class);
		Predicate predicateAwardNumber = builder.equal(rootAwardExpenseDetail.get("awardNumber"),awardNumber);
		Predicate predicateAccountNumber = builder.equal(rootAwardExpenseDetail.get("accountNumber"),accountNumber);
		Predicate predicateIOCode = builder.equal(rootAwardExpenseDetail.get("internalOrderCode"),internalOrderCode);
		query.where(builder.and(predicateAwardNumber, predicateAccountNumber, predicateIOCode));
		query.distinct(true);
		awardExpenseDetails = session.createQuery(query).getResultList();
		if (awardExpenseDetails != null && !awardExpenseDetails.isEmpty()) {
			return awardExpenseDetails.get(0);
		}
		return null;
	}

	@Override
	public void deleteAwardExpenseDetailsExtById(AwardExpenseDetailsExt awardExpenseDetailsExt) {
		hibernateTemplate.delete(awardExpenseDetailsExt);
	}

	@Override
	public AwardExpenseDetailsExt getAwardExpenseDetailsExtById(Integer awardExpenseDetailsId) {
		return hibernateTemplate.get(AwardExpenseDetailsExt.class, awardExpenseDetailsId);
	}

	@SuppressWarnings("unchecked")
	@Override
	public AwardExpenseHeader getAwardExpenseHeader(String awardNumber, String accountNumber) {
		AwardExpenseHeader awardExpenseHeader = null;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String expenseHeaderQuery = "FROM AwardExpenseHeader WHERE awardNumber=:awardNumber and accountNumber=:accountNumber";
		Query<AwardExpenseHeader> query = session.createQuery(expenseHeaderQuery);
		query.setParameter("awardNumber", awardNumber);
		query.setParameter("accountNumber", accountNumber);
		try {
			awardExpenseHeader = query.getSingleResult();
		} catch (Exception e) {
			return awardExpenseHeader;
		}
		return awardExpenseHeader;
	}

	@Override
	public ResultSet loadExpenseDetailsAsGroup(String awardNumber, String accountNumber, String type) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet rset = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				if (type.equals("E")) {
					statement = connection.prepareCall("{call get_awd_expnse_grp_costelement(?,?)}");
				} else {
					statement = connection.prepareCall("{call get_awd_purchase_grp_costelement(?,?)}");
				}
				statement.setString(1, awardNumber);
				statement.setString(2, accountNumber);
				statement.execute();
				rset = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				if (type.equals("E")) {
					statement = connection.prepareCall("{call get_awd_expnse_grp_costelement(?,?,?)}");
				} else {
					statement = connection.prepareCall("{call get_awd_purchase_grp_costelement(?,?,?)}");
				}
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, awardNumber);
				statement.setString(3, accountNumber);
				statement.execute();
				rset = (ResultSet) statement.getObject(1);
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return rset;
	}

	@Override
	public ResultSet loadAwardExpensePersonDetails(Integer budgetDetailId, String awardNumber, String accountNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet rset = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call get_award_expense_person_dtls(?,?,?)}");
				statement.setInt(1, budgetDetailId);
				statement.setString(2, awardNumber);
				statement.setString(3, accountNumber);
				statement.execute();
				rset = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String functionCall = "{call get_award_expense_person_dtls(?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.setInt(1, budgetDetailId);
				statement.setString(2, awardNumber);
				statement.setString(3, accountNumber);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.execute();
				rset = (ResultSet) statement.getObject(1);
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return rset;
	}

	@Override
	public ResultSet fetchExpenseTransactionsDetails(String accountNumber, String internalOrderCode, String actualOrCommittedFlag, String awardNumber, String fmPostingStartDate, String fmPostingEndDate, String isShowAllTransactions) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet rset = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_AWARD_EXPENSE_TRACKIG_DTLS(?,?,?,?,?,?,?)}");
				statement.setString(1, accountNumber);
				statement.setString(2, internalOrderCode);
				statement.setString(3, actualOrCommittedFlag);
				statement.setString(4, awardNumber);
				statement.setString(5, fmPostingStartDate);
				statement.setString(6, fmPostingEndDate);
				statement.setString(7, isShowAllTransactions);
				statement.execute();
				rset = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String functionCall = "{call GET_AWARD_EXPENSE_TRACKIG_DTLS(?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.setString(1, accountNumber);
				statement.setString(2, internalOrderCode);
				statement.setString(3, actualOrCommittedFlag);
				statement.setString(4, awardNumber);
				statement.setString(5, fmPostingStartDate);
				statement.setString(6, fmPostingEndDate);
				statement.setString(7, isShowAllTransactions);
				statement.registerOutParameter(5, OracleTypes.CURSOR);
				statement.execute();
				rset = (ResultSet) statement.getObject(1);
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return rset;
	
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<AwardExpenseDetailsExt> fetchAwardExpenseDetailsExtByParams(String awardNumber, String accountNumber,
			List<String> ioCodes) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		if (ioCodes == null || ioCodes.isEmpty()) {
			String hqlQuery = "FROM AwardExpenseDetailsExt WHERE awardNumber=:awardNumber and accountNumber=:accountNumber  and isFromSap=:isFromSap";
			Query<AwardExpenseDetailsExt> query = session.createQuery(hqlQuery);
			query.setParameter("awardNumber", awardNumber);
			query.setParameter("accountNumber", accountNumber);
			query.setParameter("isFromSap", "Y");
			return query.getResultList();
		} else {
			String hqlQuery = "FROM AwardExpenseDetailsExt WHERE awardNumber=:awardNumber and accountNumber=:accountNumber "
					+ "and (internalOrderCode not in (:internalOrderCode) or internalOrderCode is null )and isFromSap=:isFromSap";
			Query<AwardExpenseDetailsExt> query = session.createQuery(hqlQuery);
			query.setParameter("awardNumber", awardNumber);
			query.setParameter("accountNumber", accountNumber);
			query.setParameter("internalOrderCode", ioCodes);
			query.setParameter("isFromSap", "Y");
			return query.getResultList();
		}
	
	}

	@Override
	public AwardExpenseDetailsExt getAwardExpenseExtDetailsByParams(String awardNumber, String accountNumber, String internalOrderCode) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AwardExpenseDetailsExt> query = builder.createQuery(AwardExpenseDetailsExt.class);
			Root<AwardExpenseDetailsExt> rootAwardExpenseDetailExt = query.from(AwardExpenseDetailsExt.class);
			Predicate predicateAwardNumber = builder.equal(rootAwardExpenseDetailExt.get("awardNumber"), awardNumber);
			Predicate predicateAccountNumber = builder.equal(rootAwardExpenseDetailExt.get("accountNumber"), accountNumber);
			Predicate predicateIOCode = builder.equal(rootAwardExpenseDetailExt.get("internalOrderCode"), internalOrderCode);
			query.where(builder.and(predicateAwardNumber, predicateAccountNumber, predicateIOCode));
			query.distinct(true);
			return session.createQuery(query).getSingleResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public List<AwardExpenseTransaction> getAwardExpenseTransactionBasedOnClaims(String awardNumber, String accountNumber, Timestamp startDate, Timestamp endDate, String actualOrCommittedFlag, List<Integer> awardExpenseTransactionId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardExpenseTransaction> query = builder.createQuery(AwardExpenseTransaction.class);
		Root<AwardExpenseTransaction> rootAwardExpenseTransaction = query.from(AwardExpenseTransaction.class);
		Predicate predicateOne = builder.equal(rootAwardExpenseTransaction.get("awardNumber"),awardNumber);
		Predicate predicateTwo = builder.equal(rootAwardExpenseTransaction.get("accountNumber"),accountNumber);
		Predicate predicateThree = builder.equal(rootAwardExpenseTransaction.get("actualOrCommittedFlag"),actualOrCommittedFlag);
		Predicate predicateFour = builder.between(rootAwardExpenseTransaction.get("fmPostingDate"),startDate, endDate);
		if (actualOrCommittedFlag != null && actualOrCommittedFlag.equals("A")) {
			Predicate predicateFive = rootAwardExpenseTransaction.get("awardExpenseTransactionId").in(awardExpenseTransactionId);
			query.where(builder.and(predicateOne, predicateTwo, predicateThree, predicateFour, predicateFive));
		} else if (actualOrCommittedFlag != null && actualOrCommittedFlag.equals("C")) {
			query.where(builder.and(predicateOne, predicateTwo, predicateThree, predicateFour));
		}
		return session.createQuery(query).getResultList();
	
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<AwardExpenseTransaction> getAwardExpenseCommittedTransactionForClaim(String awardNumber,
			String accountNumber, Timestamp startDate, Timestamp endDate) {	
		String hqlQuery = new StringBuilder("FROM AwardExpenseTransaction WHERE awardNumber =:awardNumber and accountNumber =:accountNumber ").
				append(" and actualOrCommittedFlag = 'C' and fmPostingDate <= :endDate and awardExpenseTransactionId in (select awardExpenseTransactionId").
				append(" from AwardExpenseTransaction where (documentNumber,itemNumber,referenceDocumentCategory,referenceOrgUnit,acctAssignmentNumber,scheduleLineNumber,conditionCounter,").
				append(" referenceProcedure,documentNumberFMLineItem,transactionNumber) in (select documentNumber,itemNumber,referenceDocumentCategory,referenceOrgUnit,acctAssignmentNumber,").
				append(" scheduleLineNumber,conditionCounter,referenceProcedure,documentNumberFMLineItem,transactionNumber from ExpenseZeroExcludeCommittedV)) ").toString();
		Query<AwardExpenseTransaction> query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery);
		query.setParameter("awardNumber", awardNumber);
		query.setParameter("accountNumber", accountNumber);
		query.setParameter("endDate", endDate);
		return query.getResultList();
	}
}
