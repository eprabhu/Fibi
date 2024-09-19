package com.polus.fibicomp.coi.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.polus.core.applicationexception.dto.ApplicationException;
import com.polus.core.common.dao.CommonDao;
import com.polus.core.security.AuthenticatedUser;
import com.polus.fibicomp.coi.dto.DisclosureProjectDto;
import com.polus.fibicomp.coi.dto.ProjectCommentDto;
import com.polus.fibicomp.coi.dto.ProjectStatusLookupDto;
import com.polus.fibicomp.coi.pojo.CoiProjectComment;
import com.polus.fibicomp.coi.pojo.CoiProposalStatusType;
import com.polus.fibicomp.coi.vo.CoiDashboardVO;
import com.polus.fibicomp.constants.Constants;

import oracle.jdbc.OracleTypes;

@Repository
@Transactional
public class ProjectDaoImpl implements ProjectDao {

	protected static Logger logger = LogManager.getLogger(ProjectDaoImpl.class.getName());

	private static final String DISCLOSURE_NOT_REQUIRED = "Disclosure Not Required";
	private static final String DISCLOSURE_REQUIRED = "Disclosure Required";
	private static final String PROJECT_STATUS_CODE = "statusCode";
	private static final String PROJECT_STATUS_DESCRIPTION = "description";

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private CommonDao commonDao;

	@Value("${oracledb}")
	private String oracledb;

	@Override
	public void saveComment(CoiProjectComment comment) {
		hibernateTemplate.save(comment);
	}

	@Override
	public void updateComment(ProjectCommentDto dto) {
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		hqlQuery.append("UPDATE CoiProjectComment c SET ");
		hqlQuery.append("c.updateTimestamp = :updateTimestamp, ");
		hqlQuery.append("c.updatedBy = :updatedBy, ");
		hqlQuery.append("c.comment = :comment ");
		if (dto.getIsPrivate() != null)
			hqlQuery.append(", c.isPrivate = :isPrivate ");
		hqlQuery.append("WHERE c.commentId = :commentId");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("commentId", dto.getCommentId());
		if (dto.getIsPrivate() != null)
			query.setParameter("isPrivate", dto.getIsPrivate());
		query.setParameter("comment", dto.getComment());
		query.setParameter("updateTimestamp", commonDao.getCurrentTimestamp());
		query.setParameter("updatedBy", AuthenticatedUser.getLoginUserName());
		query.executeUpdate();
	}

	@Override
	public List<CoiProjectComment> fetchComment(ProjectCommentDto dto) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaQuery<CoiProjectComment> cq = cb.createQuery(CoiProjectComment.class);
		Root<CoiProjectComment> root = cq.from(CoiProjectComment.class);
		List<Predicate> predicates = new ArrayList<>();
		predicates.add(cb.equal(root.get("moduleCode"), dto.getModuleCode()));
		predicates.add(cb.equal(root.get("moduleItemKey"), dto.getModuleItemKey()));
		if (dto.getCommentTypeCode() != null) {
			predicates.add(cb.equal(root.get("commentTypeCode"), dto.getCommentTypeCode()));
		}
		cq.select(root).where(predicates.toArray(new Predicate[0]));
		return session.createQuery(cq).getResultList();
	}

	@Override
	public void deleteComment(Integer commentId) {
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		hqlQuery.append("DELETE FROM CoiProjectComment c WHERE c.commentId = :commentId");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("commentId", commentId);
		query.executeUpdate();
	}

	@Override
	public Boolean canDeleteComment(Integer commentId) {
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		hqlQuery.append("SELECT case when (count(c.commentId) > 0) then false else true end ");
		hqlQuery.append("FROM CoiProjectComment c WHERE c.parentCommentId = :commentId");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("commentId", commentId);
		return (Boolean) query.getSingleResult();
	}

	@Override
	public List<DisclosureProjectDto> fetchProjectOverview(CoiDashboardVO vo) {
		List<DisclosureProjectDto> projectOverviewList = new ArrayList<>();
		ResultSet rset = fetchProjectOverview(vo, Boolean.FALSE);
		try {
			while (rset != null && rset.next()) {
				DisclosureProjectDto projectOverview = DisclosureProjectDto.builder()
						.projectNumber(rset.getString("PROJECT_NUMBER"))
						.projectId(rset.getString("PROJECT_ID")).title(rset.getString("TITLE"))
						.piName(rset.getString("PI_NAME")).keyPersonId(rset.getString("KEY_PERSON_ID"))
						.keyPersonName(rset.getString("KEY_PERSON_NAME"))
						.keyPersonRole(rset.getString("KEY_PERSON_ROLE")).sponsorName(rset.getString("SPONSOR_NAME"))
						.sponsorCode(rset.getString("SPONSOR_CODE"))
						.primeSponsorName(rset.getString("PRIME_SPONSOR_NAME"))
						.primeSponsorCode(rset.getString("PRIME_SPONSOR_CODE"))
						.leadUnitName(rset.getString("LEAD_UNIT_NAME"))
						.leadUnitNumber(rset.getString("LEAD_UNIT_NUMBER"))
						.homeUnitName(rset.getString("HOME_UNIT_NAME")).homeUnitNumber(rset.getString("HOME_UNIT"))
						.projectStartDate(rset.getTimestamp("PROPOSAL_START_DATE"))
						.projectEndDate(rset.getTimestamp("PROPOSAL_END_DATE"))
						.projectStatus(rset.getString("PROJECT_STATUS"))
						.disclosureSubmitted(rset.getString("DISCLOSURE_COMPLETION_STATUS").equals("Yes") ? Boolean.TRUE
								: Boolean.FALSE)
						.disclosureRequiredFlag(rset.getString("DISCLOSURE_REQUIRED_FLAG"))
						.certificationFlag(rset.getString("CERTIFICATION_FLAG"))
						.disclosureReviewStatus(rset.getString("DISCLOSURE_REVIEW_STATUS"))
						.disclosureId(rset.getInt("DISCLOSURE_ID")).projectType(rset.getString("PROJECT_TYPE"))
						.projectTypeCode(rset.getString("PROJECT_TYPE_CODE"))
						.projectBadgeColour(rset.getString("BADGE_COLOR"))
						.projectIcon(rset.getString("PROJECT_ICON"))
						.updateTimestamp(rset.getTimestamp("UPDATE_TIMESTAMP"))
						.commentCount(rset.getInt("COMMENT_COUNT")).build();
				projectOverviewList.add(projectOverview);
			}
		} catch (Exception e) {
			logger.error("Exception on fetchProjectOverview {}", e.getMessage());
			throw new ApplicationException("Unable to fetch project overview details", e, Constants.DB_PROC_ERROR);
		}
		return projectOverviewList;
	}

	private ResultSet fetchProjectOverview(CoiDashboardVO vo, Boolean isCount) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet rset = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_PROJECT_OVERVIEW(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, vo.getFilterType());
				statement.setString(2, vo.getProperty2()); // AV_PROPOSAL_TITLE
				statement.setString(3, vo.getProperty6()); // AV_PROPOSAL_NUMBER
				statement.setString(4, vo.getProperty3()); // AV_UNIT_NUMBER
				statement.setString(5, vo.getPersonId()); // AV_PERSON_ID
				statement.setString(6, (vo.getProperty4() != null && !vo.getProperty4().isEmpty())
								? String.join(",", vo.getProperty4())
								: null); // AV_PROPOSAL_STATUS
				statement.setString(7, (vo.getProperty5() != null && !vo.getProperty5().isEmpty())
								? String.join(",", vo.getProperty5())
								: null); // AV_REVIEW_STATUS
				statement.setString(8, vo.getProperty9()); // AV_SPONSOR
				statement.setString(9, vo.getProperty11()); // AV_PRIME_SPONSOR
				statement.setString(10, vo.getProperty13()); // AV_PROPOSAL_START_DATE
				statement.setString(11, vo.getProperty14()); // AV_PROPOSAL_END_DATE;
				statement.setInt(12, (vo.getCurrentPage() == null ? 1 : vo.getCurrentPage() - 1));
				statement.setInt(13, (vo.getPageNumber() == null ? 20 : vo.getPageNumber()));
				statement.setBoolean(14, isCount);
				statement.setString(15, vo.getAdvancedSearch());
				statement.execute();
				rset = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String functionCall = "{call GET_PROJECT_OVERVIEW(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, vo.getFilterType());
				statement.setString(3, vo.getProperty2());
				statement.setString(4, vo.getProperty6());
				statement.setString(5, vo.getProperty3());
				statement.setString(6, vo.getPersonId());
				statement.setString(7, (vo.getProperty4() != null && !vo.getProperty4().isEmpty())
								? String.join(",", vo.getProperty4())
								: null);
				statement.setString(8, (vo.getProperty5() != null && !vo.getProperty5().isEmpty())
								? String.join(",", vo.getProperty5())
								: null);
				statement.setString(9, vo.getProperty9());
				statement.setString(10, vo.getProperty11());
				statement.setString(11, vo.getProperty13());
				statement.setString(12, vo.getProperty14());
				statement.setInt(13, (vo.getCurrentPage() == null ? 1 : vo.getCurrentPage() - 1));
				statement.setInt(14, (vo.getPageNumber() == null ? 20 : vo.getPageNumber()));
				statement.setBoolean(15, Boolean.FALSE);
				statement.setString(16, vo.getAdvancedSearch());
				statement.execute();
				rset = (ResultSet) statement.getObject(1);
			}
		} catch (Exception e) {
			logger.error("Exception on fetchProjectOverview {}", e.getMessage());
			throw new ApplicationException("Unable to fetch project overview details", e, Constants.DB_PROC_ERROR);
		}
		return rset;
	}

	@Override
	public Integer fetchProjectOverviewCount(CoiDashboardVO vo) {
		ResultSet rset = fetchProjectOverview(vo, vo.getIsDownload());
		try {
			while (rset != null && rset.next()) {
				return rset.getInt("PROPOSAL_COUNT");
			}
		} catch (Exception e) {
			logger.error("Exception on fetchProjectOverview {}", e.getMessage());
			throw new ApplicationException("Unable to fetch project overview details", e, Constants.DB_PROC_ERROR);
		}
		return null;
	}

	@Override
	public List<ProjectStatusLookupDto> getProposalStatusLookup() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaQuery<ProjectStatusLookupDto> cq = cb.createQuery(ProjectStatusLookupDto.class);
		Root<CoiProposalStatusType> root = cq.from(CoiProposalStatusType.class);
		cq.select(cb.construct(ProjectStatusLookupDto.class, root.get(PROJECT_STATUS_CODE), root.get(PROJECT_STATUS_DESCRIPTION)));
		cq.orderBy(cb.asc(root.get(PROJECT_STATUS_CODE)));
		return session.createQuery(cq).getResultList();
	}

}
