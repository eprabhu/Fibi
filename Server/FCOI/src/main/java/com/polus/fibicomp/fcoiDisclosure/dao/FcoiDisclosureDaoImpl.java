package com.polus.fibicomp.fcoiDisclosure.dao;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.type.CollectionType;
import com.polus.core.applicationexception.dto.ApplicationException;
import com.polus.core.common.dao.CommonDao;
import com.polus.core.constants.CoreConstants;
import com.polus.core.security.AuthenticatedUser;
import com.polus.fibicomp.coi.dto.*;
import com.polus.fibicomp.coi.pojo.CoiConflictHistory;
import com.polus.fibicomp.fcoiDisclosure.dto.ProjectEntityRequestDto;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiConflictStatusType;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiDisclosureFcoiType;
import com.polus.fibicomp.coi.pojo.CoiSectionsType;
import com.polus.fibicomp.coi.vo.ConflictOfInterestVO;
import com.polus.fibicomp.fcoiDisclosure.dto.SFIJsonDetailsDto;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiDisclProjectEntityRel;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiDisclProjects;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiDisclosure;
import com.polus.fibicomp.coi.pojo.CoiProjConflictStatusType;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiRiskCategory;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.reviewcomments.pojos.DisclComment;
import oracle.jdbc.OracleTypes;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Repository;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;
import javax.persistence.criteria.Root;
import javax.transaction.Transactional;
import java.sql.*;
import java.util.*;
import java.util.stream.Collectors;

@Repository
@Transactional
public class FcoiDisclosureDaoImpl implements FcoiDisclosureDao {

    protected static Logger logger = LogManager.getLogger(FcoiDisclosureDaoImpl.class.getName());

    @Autowired
    private HibernateTemplate hibernateTemplate;

    @Autowired
    private CommonDao commonDao;

    @Value("${oracledb}")
    private String oracledb;

    @Override
    public CoiDisclosure saveOrUpdateCoiDisclosure(CoiDisclosure coiDisclosure) {
        hibernateTemplate.saveOrUpdate(coiDisclosure);
        return coiDisclosure;
    }

    @Override
    public CoiDisclosure loadDisclosure(Integer disclosureId) {
        return hibernateTemplate.get(CoiDisclosure.class, disclosureId);
    }

    @Override
    public boolean isDisclosureRiskAdded(CoiDisclosureDto coiDisclosureDto) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT case when (count(d.disclosureId) > 0) then true else false end  ");
        hqlQuery.append("FROM CoiDisclosure d WHERE d.riskCategoryCode = :riskCategoryCode AND ");
        hqlQuery.append("d.disclosureId = :disclosureId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("disclosureId", coiDisclosureDto.getDisclosureId());
        query.setParameter("riskCategoryCode", coiDisclosureDto.getRiskCategoryCode());
        return (boolean) query.getSingleResult();
    }

    @Override
    public CoiRiskCategory getRiskCategoryStatusByCode(String riskCategoryCode) {
        return hibernateTemplate.get(CoiRiskCategory.class, riskCategoryCode);
    }

    @Override
    public Timestamp updateDisclosureRiskCategory(CoiDisclosureDto coiDisclosureDto) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("UPDATE CoiDisclosure d SET d.updateTimestamp = :updateTimestamp, d.riskCategoryCode = :riskCategoryCode, ");
        hqlQuery.append("d.updatedBy = :updatedBy where d.disclosureId = :disclosureId");
        Timestamp updateTimestamp = commonDao.getCurrentTimestamp();
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("disclosureId", coiDisclosureDto.getDisclosureId());
        query.setParameter("riskCategoryCode", coiDisclosureDto.getRiskCategoryCode());
        query.setParameter("updateTimestamp", updateTimestamp);
        query.setParameter("updatedBy", AuthenticatedUser.getLoginPersonId());
        query.executeUpdate();
        return updateTimestamp;
    }

    @Override
    public List<CoiRiskCategory> fetchDisclosureRiskCategory() {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        Query query = session.createQuery("SELECT t FROM CoiRiskCategory t ORDER BY t.sortOrder ASC");
        return query.getResultList();
    }

    @Override
    public Boolean isDisclosureRiskStatusModified(String riskCategoryCode, Integer disclosureId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT case when (count(d.disclosureId) > 0) then false else true end FROM CoiDisclosure d WHERE ");
        if (riskCategoryCode != null) {
            hqlQuery.append(" d.riskCategoryCode = :riskCategoryCode AND ");
        }
        hqlQuery.append("d.disclosureId = :disclosureId ");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("disclosureId", disclosureId);
        if (riskCategoryCode != null) {
            query.setParameter("riskCategoryCode", riskCategoryCode);
        }
        return (Boolean) query.getSingleResult();
    }


    @Override
    public List<DisclosureProjectDto> getDisclosureProjects(Integer disclosureId) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        List<DisclosureProjectDto> disclosureProjects = new ArrayList<>();
        SessionImpl sessionImpl = (SessionImpl) session;
        Connection connection = sessionImpl.connection();
        CallableStatement statement;
        ResultSet rset = null;
        try {
            if (oracledb.equalsIgnoreCase("N")) {
                statement = connection.prepareCall("{call GET_DISCLOSURE_PROJECTS(?)}");
                if (disclosureId == null) {
                    statement.setNull(1, Types.INTEGER);
                } else {
                    statement.setInt(1, disclosureId);
                }
                statement.execute();
                rset = statement.getResultSet();
            } else if (oracledb.equalsIgnoreCase("Y")) {
                String procedureName = "GET_DISCLOSURE_PROJECTS";
                String functionCall = "{call " + procedureName + "(?,?)}";
                statement = connection.prepareCall(functionCall);
                statement.registerOutParameter(1, OracleTypes.CURSOR);
                if (disclosureId == null) {
                    statement.setNull(2, Types.INTEGER);
                } else {
                    statement.setInt(2, disclosureId);
                }
                statement.execute();
                rset = (ResultSet) statement.getObject(1);
            }
            while (rset != null && rset.next()) {
                disclosureProjects.add(DisclosureProjectDto.builder()
                        .coiDisclProjectId(rset.getInt("COI_DISCL_PROJECTS_ID"))
                        .moduleCode(rset.getInt("MODULE_CODE"))
                        .projectId(rset.getString("PROJECT_ID"))
                        .projectNumber(rset.getString("PROJECT_NUMBER"))
                        .title(rset.getString("PROJECT_TITLE"))
                        .projectStatus(rset.getString("PROJECT_STATUS"))
                        .projectStartDate(rset.getTimestamp("PROJECT_START_DATE"))
                        .projectEndDate(rset.getTimestamp("PROJECT_END_DATE"))
                        .homeUnitNumber(rset.getString("LEAD_UNIT_NUMBER"))
                        .homeUnitName(rset.getString("LEAD_UNIT_NAME"))
                        .sponsorName(rset.getString("PROJECT_SPONSOR_NAME"))
                        .sponsorCode(rset.getString("SPONSOR_CODE"))
                        .primeSponsorName(rset.getString("PROJECT_PRIME_SPONSOR_NAME"))
                        .primeSponsorCode(rset.getString("PRIME_SPONSOR_CODE"))
                        .piName(rset.getString("PI_NAME"))
                        .keyPersonId(rset.getString("KEY_PERSON_ID"))
                        .keyPersonName(rset.getString("KEY_PERSON_NAME"))
                        .reporterRole(rset.getString("KEY_PERSON_ROLE_NAME"))
                        .projectType(rset.getString("COI_PROJECT_TYPE"))
                        .projectTypeCode(rset.getString("COI_PROJECT_TYPE_CODE"))
                        .projectBadgeColour(rset.getString("BADGE_COLOR"))
                        .projectIcon(rset.getString("PROJECT_ICON"))
                        .build());
            }
        } catch (SQLException e) {
            logger.error("Exception in getDisclosureProjects: {} ", e.getMessage());
            throw new ApplicationException("Exception in getDisclosureProjects", e, Constants.DB_PROC_ERROR);
        }
        return disclosureProjects;
    }

    @Override
    public List<Map<Object, Object>> convertJsonStringToListMap(String jsonString) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            mapper.configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true);
            mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
            CollectionType listType = mapper.getTypeFactory().constructCollectionType(List.class, Map.class);
            return mapper.readValue(jsonString, listType);
        } catch (Exception e) {
            e.printStackTrace();
            logger.error("Exception in convertJsonStringToListMap", e.getMessage());
        }
        return null;
    }

    @Override
    public List<CoiConflictStatusType> getCoiConflictStatusTypes() {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        Query query = session.createQuery("SELECT t FROM CoiConflictStatusType t ORDER BY t.sortOrder ASC");
        return query.getResultList();
    }

    @Override
    public List<CoiProjConflictStatusType> getProjConflictStatusTypes() {
        return hibernateTemplate.loadAll(CoiProjConflictStatusType.class);
    }

    @Override
    public boolean isMasterDisclosurePresent(String personId) {
        try {
            Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
            CriteriaBuilder builder = session.getCriteriaBuilder();
            CriteriaQuery<Long> query = builder.createQuery(Long.class);
            Root<CoiDisclosure> rootCoiDisclosure = query.from(CoiDisclosure.class);
            query.select(builder.count(rootCoiDisclosure));
            query.where(builder.and(builder.equal(rootCoiDisclosure.get("personId"), personId),
                    builder.equal(rootCoiDisclosure.get("fcoiTypeCode"), "1"),
                    builder.equal(rootCoiDisclosure.get("versionStatus"), Constants.COI_ACTIVE_STATUS)));
            Long count = session.createQuery(query).getSingleResult();
            return count > 0 ? true : false;
        } catch (Exception ex) {
            return false;
        }
    }

    @Override
    public Integer generateMaxDisclosureNumber() {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        CriteriaBuilder builder = session.getCriteriaBuilder();
        CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
        Root<CoiDisclosure> root = query.from(CoiDisclosure.class);
        query.select(builder.max(root.get("disclosureNumber")));
        if (session.createQuery(query).getSingleResult() != null) {
            return session.createQuery(query).getSingleResult() + 1;
        } else {
            return 1;
        }
    }

    @Override
    public CoiDisclosureFcoiType getCoiDisclosureFcoiTypeByCode(String coiTypeCode) {
        return hibernateTemplate.get(CoiDisclosureFcoiType.class, coiTypeCode);
    }

    @Override
    public List<CoiSectionsType> fetchCoiSections() {
        return hibernateTemplate.loadAll(CoiSectionsType.class);
    }

    @Override
    public Boolean isReviewerAssigned(Integer disclosureId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT COUNT(r.coiReviewId) FROM CoiReview r ");
        hqlQuery.append("WHERE r.disclosureId = :disclosureId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("disclosureId", disclosureId);
        Long count = (Long) query.getSingleResult();
        return count > 0;
    }

    @Override
    public Boolean isReviewerReviewCompleted(Integer disclosureId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT COUNT(r.coiReviewId) FROM CoiReview r ");
        hqlQuery.append("WHERE r.reviewStatusTypeCode <> 2 AND r.disclosureId = :disclosureId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("disclosureId", disclosureId);
        Long count = (Long) query.getSingleResult();
        return (count <= 0);
    }

    @Override
    public void certifyDisclosure(CoiDisclosureDto coiDisclosure) {
        Timestamp currentTimestamp = commonDao.getCurrentTimestamp();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        CriteriaBuilder cb = session.getCriteriaBuilder();
        CriteriaUpdate<CoiDisclosure> criteriaUpdate = cb.createCriteriaUpdate(CoiDisclosure.class);
        Root<CoiDisclosure> root = criteriaUpdate.from(CoiDisclosure.class);
        criteriaUpdate.set("certificationText", coiDisclosure.getCertificationText());
        criteriaUpdate.set("certifiedAt", currentTimestamp);
        criteriaUpdate.set("certifiedBy", AuthenticatedUser.getLoginPersonId());
        criteriaUpdate.set("conflictStatusCode", coiDisclosure.getConflictStatusCode());
        criteriaUpdate.set("dispositionStatusCode", coiDisclosure.getDispositionStatusCode());
        criteriaUpdate.set("reviewStatusCode", coiDisclosure.getReviewStatusCode());
        criteriaUpdate.set("updateTimestamp", currentTimestamp);
        criteriaUpdate.set("updatedBy", AuthenticatedUser.getLoginPersonId());
        criteriaUpdate.set("expirationDate", coiDisclosure.getExpirationDate());
        criteriaUpdate.where(cb.equal(root.get("disclosureId"), coiDisclosure.getDisclosureId()));
        session.createQuery(criteriaUpdate).executeUpdate();
    }

    @Override
    public CoiConflictStatusTypeDto validateConflicts(Integer disclosureId) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        SessionImpl sessionImpl = (SessionImpl) session;
        Connection connection = sessionImpl.connection();
        try {
            CallableStatement statement = connection.prepareCall("{call COI_VALIDATE_DISCLOSURE_CONFLICTS(?,?,?)}");
            statement.setInt(1, disclosureId);
            statement.setString(2, AuthenticatedUser.getLoginPersonId());
            statement.setString(3, AuthenticatedUser.getLoginPersonId());
            statement.execute();
            ResultSet rset = statement.getResultSet();
            if (rset != null && rset.next()) {
                CoiConflictStatusTypeDto conflictStatusTypeDto = new CoiConflictStatusTypeDto();
                conflictStatusTypeDto.setConflictStatusCode(rset.getString(1));
                conflictStatusTypeDto.setDescription(rset.getString(2));
                return conflictStatusTypeDto;
            }
        } catch (Exception e) {
            logger.error("Exception on validateConflicts {}", e.getMessage());
            throw new ApplicationException("error in validate conflicts ", e, Constants.DB_FN_ERROR);
        }
        return null;
    }

    @Override
    public CoiRiskCategory syncDisclosureRisk(Integer disclosureId, Integer disclosureNumber) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        SessionImpl sessionImpl = (SessionImpl) session;
        Connection connection = sessionImpl.connection();
        try {
            CallableStatement statement = connection.prepareCall("{call COI_SYNC_DISCLOSURE_RISK(?,?,?)}");
            statement.setInt(1, disclosureId);
            statement.setInt(2, disclosureNumber);
            statement.setString(3, AuthenticatedUser.getLoginUserName());
            statement.execute();
            ResultSet rset = statement.getResultSet();
            if (rset != null && rset.next()) {
                CoiRiskCategory riskCategory = new CoiRiskCategory();
                riskCategory.setRiskCategoryCode(rset.getString(1));
                riskCategory.setDescription(rset.getString(2));
                return riskCategory;
            }
        } catch (Exception e) {
            logger.error("Exception on syncDisclosureRisk {}", e.getMessage());
        }
        return null;
    }

    @Override
    public List<CoiDisclProjectEntityRel> getProjEntityRelationshipsByDisclId(Integer disclosureId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT r FROM CoiDisclProjectEntityRel r ");
        hqlQuery.append("WHERE r.coiDisclProject.disclosureId = :disclosureId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("disclosureId", disclosureId);
        return query.getResultList();
    }

    @Override
    public String getLatestConflHisStatusCodeByProEntRelId(Integer coiDisclProjectEntityRelId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT cs.description FROM CoiConflictHistory ch ");
        hqlQuery.append("JOIN CoiProjConflictStatusType cs ON cs.projectConflictStatusCode = ch.conflictStatusCode ");
        hqlQuery.append("WHERE ch.coiDisclProjectEntityRelId = :coiDisclProjectEntityRelId ORDER BY ch.updateTimestamp DESC");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("coiDisclProjectEntityRelId", coiDisclProjectEntityRelId);
        query.setMaxResults(1);
        List<String> resultData = query.getResultList();
        return resultData != null && !resultData.isEmpty()? (String) resultData.get(0) : "";
    }

    @Override
    public void saveOrUpdateCoiConflictHistory(CoiConflictHistory coiConflictHistory) {
        hibernateTemplate.saveOrUpdate(coiConflictHistory);
//        return coiConflictHistory;
    }

    @Override
    public void saveOrUpdateCoiDisclEntProjDetails(ProjectEntityRequestDto entityProjectRelation) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("UPDATE COI_DISCL_PROJECT_ENTITY_REL cd ");
        hqlQuery.append("INNER JOIN COI_DISCL_PROJECTS d ON d.COI_DISCL_PROJECTS_ID = cd.COI_DISCL_PROJECTS_ID ");
        hqlQuery.append("SET cd.PROJECT_CONFLICT_STATUS_CODE = :projectConflictStatusCode, ");
        hqlQuery.append("cd.UPDATED_BY = :updatedBy, cd.UPDATE_TIMESTAMP = :updateTimestamp, cd.PREVIOUS_PERSON_ENTITY_ID = cd.PERSON_ENTITY_ID ");
        hqlQuery.append("WHERE d.DISCLOSURE_ID = :disclosureId ");
        if (entityProjectRelation.getApplyAll()) {
            if (entityProjectRelation.getRelationshipSFIMode()) {
                hqlQuery.append("AND cd.PERSON_ENTITY_ID = :personEntityId ");
            } else {
                hqlQuery.append("AND cd.COI_DISCL_PROJECTS_ID = :coiDisclProjectId ");
            }
        } else {
            hqlQuery.append("AND cd.COI_DISCL_PROJECT_ENTITY_REL_ID = :coiDisclProjectEntityRelId ");
        }
        Query query = session.createNativeQuery(hqlQuery.toString());
        if (entityProjectRelation.getApplyAll()) {
            if (entityProjectRelation.getRelationshipSFIMode()) {
                query.setParameter("personEntityId", entityProjectRelation.getPersonEntityId());
            } else {
                query.setParameter("coiDisclProjectId", entityProjectRelation.getCoiDisclProjectId());
            }
        } else {
            query.setParameter("coiDisclProjectEntityRelId", entityProjectRelation.getCoiDisclProjectEntityRelId());
        }
        query.setParameter("disclosureId", entityProjectRelation.getDisclosureId());
        query.setParameter("projectConflictStatusCode", entityProjectRelation.getProjectConflictStatusCode());
        query.setParameter("updatedBy", AuthenticatedUser.getLoginPersonId());
        query.setParameter("updateTimestamp", commonDao.getCurrentTimestamp());
        query.executeUpdate();
    }

    @Override
    public List<Object[]> fetchDisclProjectEntityRelIds(ProjectEntityRequestDto entityProjectRelation) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT r.COI_DISCL_PROJECT_ENTITY_REL_ID, c.COMMENT_ID FROM COI_DISCL_PROJECT_ENTITY_REL r ");
        hqlQuery.append("LEFT JOIN (SELECT SUB_MODULE_ITEM_KEY, COMMENT_ID  FROM DISCL_COMMENT WHERE MODULE_CODE = 8 ");
        hqlQuery.append("AND MODULE_ITEM_KEY = :disclosureId AND MODULE_ITEM_NUMBER = :moduleItemNumber ");
        hqlQuery.append("AND COMPONENT_TYPE_CODE = 1 ) c ON c.SUB_MODULE_ITEM_KEY = r.COI_DISCL_PROJECT_ENTITY_REL_ID ");
        hqlQuery.append("INNER JOIN COI_DISCL_PROJECTS p ON p.COI_DISCL_PROJECTS_ID = r.COI_DISCL_PROJECTS_ID ");
        hqlQuery.append("WHERE p.DISCLOSURE_ID = :disclosureId ");
        hqlQuery.append(" ");
        if (entityProjectRelation.getApplyAll()) {
            if (entityProjectRelation.getRelationshipSFIMode()) {
                hqlQuery.append("AND r.PERSON_ENTITY_ID = :personEntityId ");
            } else {
                hqlQuery.append("AND r.COI_DISCL_PROJECTS_ID = :coiDisclProjectId ");
            }
        }
        Query query = session.createNativeQuery(hqlQuery.toString());
        if (entityProjectRelation.getApplyAll()) {
            if (entityProjectRelation.getRelationshipSFIMode()) {
                query.setParameter("personEntityId", entityProjectRelation.getPersonEntityId());
            } else {
                query.setParameter("coiDisclProjectId", entityProjectRelation.getCoiDisclProjectId());
            }
        }
        query.setParameter("disclosureId", entityProjectRelation.getDisclosureId());
        query.setParameter("moduleItemNumber", entityProjectRelation.getDisclosureNumber());
        return query.getResultList();
    }

    @Override
    public Boolean isSFICompletedForDisclosure(Integer personEntityId, Integer disclosureId) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        StringBuilder hqlQuery = new StringBuilder();
        hqlQuery.append("SELECT COUNT(*) FROM CoiDisclProjectEntityRel r ");
        hqlQuery.append("WHERE r.projectConflictStatusCode IS NULL ");
        hqlQuery.append("AND r.coiDisclProject.disclosureId = :disclosureId ");
        hqlQuery.append("AND r.personEntityId = :personEntityId ");
        Query query = session.createQuery(hqlQuery.toString(), Long.class);
        query.setParameter("disclosureId", disclosureId);
        query.setParameter("personEntityId", personEntityId);
        Object countData = query.getSingleResult();
        if (countData != null) {
            Long count = (Long) countData;
            return count.intValue() != 0 ? Boolean.FALSE : Boolean.TRUE;
        }
        return null;
    }

    @Override
    public Boolean checkIsSFICompletedForProject(Integer moduleCode, Integer moduleItemId, Integer disclosureId) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        StringBuilder hqlQuery = new StringBuilder();
        hqlQuery.append("SELECT COUNT(*) FROM CoiDisclProjectEntityRel r ");
        hqlQuery.append("WHERE r.projectConflictStatusCode IS NULL ");
        hqlQuery.append("AND r.coiDisclProject.disclosureId = :disclosureId ");
        hqlQuery.append("AND r.coiDisclProjectId = :moduleCode ");
        Query query = session.createQuery(hqlQuery.toString(), Long.class);
        query.setParameter("disclosureId", disclosureId);
        query.setParameter("moduleCode", moduleCode);
        Object countData = query.getSingleResult();
        if (countData != null) {
            Long count = (Long) countData;
            return count.intValue() != 0 ? Boolean.FALSE : Boolean.TRUE;
        }
        return null;
    }

    @Override
    public Timestamp updateDisclosureUpdateDetails(Integer disclosureId) {
        Timestamp updateTimestamp = commonDao.getCurrentTimestamp();
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("UPDATE CoiDisclosure cd SET cd.updateTimestamp = :updateTimestamp, cd.updatedBy = :updatedBy where cd.disclosureId = :disclosureId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("disclosureId", disclosureId);
        query.setParameter("updateTimestamp", updateTimestamp);
        query.setParameter("updatedBy", AuthenticatedUser.getLoginPersonId());
        query.executeUpdate();
        return updateTimestamp;
    }

    @Override
    public List<CoiDisclEntProjDetailsDto> getDisclEntProjDetails(Integer disclosureId) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        List<CoiDisclEntProjDetailsDto> disclosureProjects = new ArrayList<>();
        SessionImpl sessionImpl = (SessionImpl) session;
        Connection connection = sessionImpl.connection();
        CallableStatement statement;
        ResultSet rset = null;
        try {
            if (oracledb.equalsIgnoreCase("N")) {
                statement = connection.prepareCall("{call COI_DISCL_ENT_PROJ_DETAILS(?)}");
                statement.setInt(1, disclosureId);
                statement.execute();
                rset = statement.getResultSet();
            } else if (oracledb.equalsIgnoreCase("Y")) {
                String procedureName = "COI_DISCL_ENT_PROJ_DETAILS";
                String functionCall = "{call " + procedureName + "(?,?)}";
                statement = connection.prepareCall(functionCall);
                statement.registerOutParameter(1, OracleTypes.CURSOR);
                statement.setInt(2, disclosureId);
                statement.execute();
                rset = (ResultSet) statement.getObject(1);
            }
            while (rset != null && rset.next()) {
                CoiDisclEntProjDetailsDto entProjDetailsDto = new CoiDisclEntProjDetailsDto();
                entProjDetailsDto.setCoiDisclProjectEntityRelId(rset.getInt("COI_DISCL_PROJECT_ENTITY_REL_ID"));
                entProjDetailsDto.setCoiDisclProjectId(rset.getInt("COI_DISCL_PROJECTS_ID"));
                entProjDetailsDto.setPersonEntityId(rset.getInt("PERSON_ENTITY_ID"));
                entProjDetailsDto.setPersonEntityNumber(rset.getInt("PERSON_ENTITY_NUMBER"));
                entProjDetailsDto.setPrePersonEntityId(rset.getInt("PREVIOUS_PERSON_ENTITY_ID"));
                entProjDetailsDto.setEntityId(rset.getInt("ENTITY_ID"));
                entProjDetailsDto.setProjectConflictStatusCode(rset.getString("PROJECT_CONFLICT_STATUS_CODE"));
                entProjDetailsDto.setUpdatedBy(rset.getString("UPDATED_BY"));
                entProjDetailsDto.setUpdateTimestamp(rset.getTimestamp("UPDATE_TIMESTAMP"));
                CoiProjConflictStatusType coiProjConflictStatusType = new CoiProjConflictStatusType();
                coiProjConflictStatusType.setDescription(rset.getString("PROJECT_CONFLICT_STATUS"));
                coiProjConflictStatusType.setProjectConflictStatusCode(rset.getString("PROJECT_CONFLICT_STATUS_CODE"));
                entProjDetailsDto.setCoiProjConflictStatusType(coiProjConflictStatusType);
                DisclComment disclComment = new DisclComment();
                disclComment.setCommentId(rset.getInt("COMMENT_ID") == 0 ? null : rset.getInt("COMMENT_ID"));
                disclComment.setComment(rset.getString("COMMENT"));
                entProjDetailsDto.setDisclComment(disclComment);
                disclosureProjects.add(entProjDetailsDto);
            }
        } catch (SQLException e) {
            logger.error("Exception in getDisclEntProjDetails: {} ", e.getMessage());
            throw new ApplicationException("Exception in getDisclEntProjDetails", e, Constants.DB_PROC_ERROR);
        }
        return disclosureProjects;
    }

    @Override
    public CoiDisclosure isFCOIDisclosureExists(String personId, String fcoiTypeCode, String versionStatus) {
        try {
            StringBuilder hqlQuery = new StringBuilder();
            Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
            hqlQuery.append("SELECT d FROM CoiDisclosure d ");
            hqlQuery.append("WHERE d.fcoiTypeCode = :fcoiTypeCode AND ");
            hqlQuery.append("d.versionStatus = :versionStatus AND d.personId = :personId");
            Timestamp updateTimestamp = commonDao.getCurrentTimestamp();
            Query query = session.createQuery(hqlQuery.toString());
            query.setParameter("fcoiTypeCode", fcoiTypeCode);
            query.setParameter("versionStatus", versionStatus);
            query.setParameter("personId", personId);
            List<CoiDisclosure> disclData = query.getResultList();
            if (disclData != null && !disclData.isEmpty()) {
                return disclData.get(0);
            }
        } catch (Exception ex) {
            logger.error("Exception in isFCOIDisclosureExists", ex);
        }
        return null;
    }

    @Override
    public boolean evaluateDisclosureQuestionnaire(Integer moduleCode, Integer submoduleCode, String moduleItemKey) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        SessionImpl sessionImpl = (SessionImpl) session;
        Connection connection = sessionImpl.connection();
        CallableStatement statement = null;
        try {
            String functionName = "FN_EVAL_DISCLOSURE_QUESTIONNAIRE";
            String functionCall = "{ ? = call " + functionName + "(?,?,?) }";
            statement = connection.prepareCall(functionCall);
            statement.registerOutParameter(1, OracleTypes.INTEGER);
            statement.setInt(2, moduleCode);
            statement.setInt(3, submoduleCode);
            statement.setString(4, moduleItemKey);
            statement.execute();
            int result = statement.getInt(1);
            if (result == 1) {
                return true;
            }
        } catch (SQLException e) {
            logger.error("Exception on evaluateDisclosureQuestionnaire {}", e.getMessage());
            throw new ApplicationException("error in evaluateDisclosureQuestionnaire", e, Constants.DB_FN_ERROR);
        }
        return false;
    }

    @Override
    public boolean isDisclEntProjConflictAdded(String projectConflictStatusCode, Integer coiDisclProjectEntityRelId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT case when (count(d.coiDisclProjectEntityRelId) > 0) then true else false end  ");
        hqlQuery.append("FROM CoiDisclProjectEntityRel d WHERE d.projectConflictStatusCode = :projectConflictStatusCode AND ");
        hqlQuery.append("d.coiDisclProjectEntityRelId = :coiDisclProjectEntityRelId");
        Timestamp updateTimestamp = commonDao.getCurrentTimestamp();
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("coiDisclProjectEntityRelId", coiDisclProjectEntityRelId);
        query.setParameter("projectConflictStatusCode", projectConflictStatusCode);
        return (boolean) query.getSingleResult();
    }

    @Override
    public CoiDisclProjectEntityRel getCoiDisclProjectEntityRelById(Integer coiDisclProjectEntityRelId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT d FROM CoiDisclProjectEntityRel d ");
        hqlQuery.append("WHERE d.coiDisclProjectEntityRelId = :coiDisclProjectEntityRelId ");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("coiDisclProjectEntityRelId", coiDisclProjectEntityRelId);
        List<CoiDisclProjectEntityRel> disclData = query.getResultList();
        if (!disclData.isEmpty()) {
            return disclData.get(0);
        }
        return null;
    }

    @Override
    public Timestamp updateCoiDisclEntProjDetails(String conflictStatusCode, Integer coiDisclProjectEntityRelId) {
        Timestamp updateTimestamp = commonDao.getCurrentTimestamp();
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("UPDATE CoiDisclProjectEntityRel cd SET cd.projectConflictStatusCode = :projectConflictStatusCode, ");
        hqlQuery.append("cd.updateTimestamp = :updateTimestamp, ");
        hqlQuery.append("cd.updatedBy = :updatedBy where cd.coiDisclProjectEntityRelId = :coiDisclProjectEntityRelId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("coiDisclProjectEntityRelId", coiDisclProjectEntityRelId);
        query.setParameter("projectConflictStatusCode", conflictStatusCode);
        query.setParameter("updateTimestamp", updateTimestamp);
        query.setParameter("updatedBy", AuthenticatedUser.getLoginPersonId());
        query.executeUpdate();
        return updateTimestamp;
    }

    @Override
    public List<CoiDisclProjectEntityRel> getProjectRelationshipByParam(Integer moduleCode, Integer moduleItemId, String loginPersonId, Integer disclosureId) {
        return hibernateTemplate.execute(session -> {
            StringBuilder hqlBuilder = new StringBuilder("SELECT DISTINCT dr FROM CoiDisclProjectEntityRel dr ");
            hqlBuilder.append("WHERE dr.coiDisclProject.coiDisclosure.personId = :loginPersonId ");
            hqlBuilder.append("AND dr.coiDisclProject.disclosureId = :disclosureId ");
            if (moduleCode != null && moduleItemId != null) {
                hqlBuilder.append("AND dr.coiDisclProject.moduleCode = :moduleCode ");
                hqlBuilder.append("AND dr.coiDisclProject.moduleItemKey = :moduleItemId ");
            }
            hqlBuilder.append("AND dr.personEntityId IS NOT NULL ");
            hqlBuilder.append("ORDER BY dr.updateTimestamp DESC");
            String hql = hqlBuilder.toString();
            org.hibernate.query.Query<CoiDisclProjectEntityRel> query = session.createQuery(hql, CoiDisclProjectEntityRel.class)
                    .setParameter("loginPersonId", loginPersonId)
                    .setParameter("disclosureId", disclosureId);
            if (moduleCode != null && moduleItemId != null) {
                query.setParameter("moduleCode", moduleCode)
                        .setParameter("moduleItemId", String.valueOf(moduleItemId));
            }
            return query.getResultList();
        });
    }

    @Override
    public Long getNumberOfSFIBasedOnDisclosureId(Integer disclosureId) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        StringBuilder hqlQuery = new StringBuilder();
        hqlQuery.append("SELECT COUNT(DISTINCT r.personEntityId) FROM CoiDisclProjectEntityRel r ");
        hqlQuery.append("WHERE r.coiDisclProject.disclosureId = :disclosureId ");
        Query query = session.createQuery(hqlQuery.toString(), Long.class);
        query.setParameter("disclosureId", disclosureId);
        Object countData = query.getSingleResult();
        if (countData != null) {
            return (Long) countData;
        }
        return null;
    }

    @Override
    public Map<String, Object> validateProjectDisclosure(String personId, Integer moduleCode, String moduleItemKey) {
        Map<String, Object> mapObj = new HashMap();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        SessionImpl sessionImpl = (SessionImpl) session;
        Connection connection = sessionImpl.connection();
        CallableStatement statement = null;
        ResultSet rset = null;
        try {
            if (oracledb.equalsIgnoreCase("N")) {
                statement = connection.prepareCall("{call VALIDATE_PROJECT_DISCLOSURE(?,?,?)}");
                statement.setString(1, personId);
                if (moduleCode == null) {
                    statement.setNull(2, Types.INTEGER);
                } else {
                    statement.setInt(2, moduleCode);
                }
                if (moduleItemKey == null) {
                    statement.setNull(3, Types.VARCHAR);
                } else {
                    statement.setString(3, moduleItemKey);
                }
                statement.execute();
                rset = statement.getResultSet();
            } else if (oracledb.equalsIgnoreCase("Y")) {
                String functionCall = "{call VALIDATE_PROJECT_DISCLOSURE(?,?,?,?)}";
                statement = connection.prepareCall(functionCall);
                statement.registerOutParameter(1, OracleTypes.CURSOR);
                statement.setString(2, personId);
                statement.setInt(3, moduleCode);
                statement.setString(4, moduleItemKey);
                statement.execute();
                rset = (ResultSet) statement.getObject(1);
            }
            while (rset.next()) {
                mapObj.put("isExpired", rset.getBoolean("isExpired"));
                mapObj.put("projectDisclosure", rset.getInt("projectDisclosure") == 0 ? null : rset.getInt("projectDisclosure"));
                mapObj.put("fcoiDisclosure", rset.getInt("fcoiDisclosure") == 0 ? null : rset.getInt("fcoiDisclosure"));
            }
        } catch (Exception e) {
            e.printStackTrace();
            logger.error("Exception on validateProjectDisclosure {}", e.getMessage());
            throw new ApplicationException("Unable to fetch disclosure", e, Constants.JAVA_ERROR);
        }
        return mapObj;
    }

    @Override
    public void saveOrUpdateCoiDisclProjects(CoiDisclProjects coiDisclProjects) {
        hibernateTemplate.saveOrUpdate(coiDisclProjects);
    }

    @Override
    public List<CoiDisclProjects> syncFcoiDisclosureProjects(Integer disclosureId, Integer disclosureNumber, String loginPersonId) {
        List<CoiDisclProjects> coiDisclProjects = new ArrayList<>();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        SessionImpl sessionImpl = (SessionImpl) session;
        Connection connection = sessionImpl.connection();
        CallableStatement statement = null;
        ResultSet rset = null;
        try {
            if (oracledb.equalsIgnoreCase("N")) {
                statement = connection.prepareCall("{call COI_SYNC_INSERT_DISCL_PROJECTS(?,?,?)}");
                statement.setInt(1, disclosureId);
                statement.setInt(2, disclosureNumber);
                statement.setString(3, loginPersonId);
                statement.execute();
                rset = statement.getResultSet();
            } else if (oracledb.equalsIgnoreCase("Y")) {
                String functionCall = "{call COI_SYNC_INSERT_DISCL_PROJECTS(?,?,?,?)}";
                statement = connection.prepareCall(functionCall);
                statement.registerOutParameter(1, OracleTypes.CURSOR);
                statement.setInt(2, disclosureId);
                statement.setInt(3, disclosureNumber);
                statement.setString(4, loginPersonId);
                statement.execute();
                rset = (ResultSet) statement.getObject(1);
            }

            while (rset.next()) {
                CoiDisclProjects disclProject = CoiDisclProjects.builder()
                        .coiDisclProjectId(rset.getInt("COI_DISCL_PROJECTS_ID"))
                        .moduleCode(rset.getInt("MODULE_CODE"))
                        .moduleItemKey(rset.getString("MODULE_ITEM_KEY"))
                        .build();
                coiDisclProjects.add(disclProject);
            }
            return coiDisclProjects;
        } catch (Exception e) {
            e.printStackTrace();
            logger.error("Exception on syncFcoiDisclosureProjects {}", e.getMessage());
            throw new ApplicationException("Exception on syncFcoiDisclosureProjects", e, Constants.DB_PROC_ERROR);
        }
    }

    @Override
    public List<SFIJsonDetailsDto> getPersonEntitiesByPersonId(String personId) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        StringBuilder hqlQuery = new StringBuilder();
        hqlQuery.append("SELECT new com.polus.fibicomp.fcoiDisclosure.dto.SFIJsonDetailsDto(");
        hqlQuery.append("pe.personEntityId, pe.personEntityNumber, pe.entityId) ");
        hqlQuery.append("FROM PersonEntity pe ");
        hqlQuery.append("JOIN PersonEntityRelationship er ON er.personEntityId = pe.personEntityId ");
        hqlQuery.append("JOIN ValidPersonEntityRelType vp ON vp.validPersonEntityRelTypeCode = er.validPersonEntityRelTypeCode ");
        hqlQuery.append("WHERE pe.personId = :personId ");
        hqlQuery.append("AND pe.versionStatus = :versionStatus ");
        hqlQuery.append("AND vp.disclosureTypeCode = :disclosureTypeCode");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("personId", personId);
        query.setParameter("versionStatus", "ACTIVE");
        query.setParameter("disclosureTypeCode", "1");
        return query.getResultList();

    }

    @Override
    public void syncFcoiDisclProjectsAndEntities(Integer disclosureId, Integer disclosureNumber, Integer coiDisclProjectId, Integer moduleCode,
                                                 String moduleItemKey, String sfiJsonArray, String loginPersonId) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        SessionImpl sessionImpl = (SessionImpl) session;
        Connection connection = sessionImpl.connection();
        CallableStatement statement = null;
//        ResultSet rset = null;
        try {
            if (oracledb.equalsIgnoreCase("N")) {
                statement = connection.prepareCall("{call COI_DISCL_PROJ_ENTITY_INSERTION(?,?,?,?,?,?,?,?)}");
                statement.setInt(1, coiDisclProjectId);
                statement.setInt(2, disclosureId);
                statement.setInt(3, disclosureNumber);
                statement.setInt(4, moduleCode);
                statement.setString(5, moduleItemKey);
                statement.setString(6, loginPersonId);
                statement.setString(7, loginPersonId);
                statement.setString(8, sfiJsonArray);
                statement.setString(8, sfiJsonArray);
            } else if (oracledb.equalsIgnoreCase("Y")) {
                String functionCall = "{call COI_DISCL_PROJ_ENTITY_INSERTION(?,?,?,?,?,?,?,?,?)}";
                statement = connection.prepareCall(functionCall);
                statement.registerOutParameter(1, OracleTypes.CURSOR);
                statement.setInt(2, coiDisclProjectId);
                statement.setInt(3, disclosureId);
                statement.setInt(4, disclosureNumber);
                statement.setInt(5, moduleCode);
                statement.setString(6, moduleItemKey);
                statement.setString(7, loginPersonId);
                statement.setString(8, loginPersonId);
                statement.setString(9, sfiJsonArray);
                statement.setString(10, sfiJsonArray);
            }

            Objects.requireNonNull(statement).execute();
            session.flush();
        } catch (Exception e) {
            logger.error("Exception on syncFcoiDisclProjectsAndEntities for coiDisclProjectId : {} | {}", coiDisclProjectId, e.getMessage());
//            throw new ApplicationException("Unable to fetch disclosure", e, Constants.DB_PROC_ERROR);
        } finally {

        }
//        return null;
    }

    @Override
    public boolean isAdminPersonOrGroupAdded(Integer disclosureId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT case when (count(c.disclosureId) > 0) then false else true end ");
        hqlQuery.append("FROM CoiDisclosure c WHERE  c.adminPersonId is null AND c.adminGroupId is null ");
        hqlQuery.append("AND c.disclosureId = : disclosureId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("disclosureId", disclosureId);
        return (boolean) query.getSingleResult();
    }

    @Override
    public boolean isSameAdminPersonOrGroupAdded(Integer adminGroupId, String adminPersonId, Integer disclosureId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT case when (count(c.disclosureId) > 0) then true else false end ");
        hqlQuery.append("FROM CoiDisclosure c WHERE  c.adminPersonId = :adminPersonId ");
        if (adminGroupId != null)
            hqlQuery.append("AND c.adminGroupId = :adminGroupId ");
        hqlQuery.append("AND c.disclosureId = : disclosureId");
        Query query = session.createQuery(hqlQuery.toString());
        if (adminGroupId != null)
            query.setParameter("adminGroupId", adminGroupId);
        query.setParameter("adminPersonId", adminPersonId);
        query.setParameter("disclosureId", disclosureId);
        return (boolean) query.getSingleResult();
    }

    @Override
    public Timestamp assignDisclosureAdmin(Integer adminGroupId, String adminPersonId, Integer disclosureId) {
        Timestamp updateTimestamp = commonDao.getCurrentTimestamp();
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("UPDATE CoiDisclosure c SET c.adminGroupId = :adminGroupId , c.adminPersonId = :adminPersonId, ");
        hqlQuery.append("c.updateTimestamp = :updateTimestamp, c.updatedBy = :updatedBy ");
        hqlQuery.append("WHERE c.disclosureId = : disclosureId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("adminGroupId", adminGroupId);
        query.setParameter("adminPersonId", adminPersonId);
        query.setParameter("disclosureId", disclosureId);
        query.setParameter("updateTimestamp", updateTimestamp);
        query.setParameter("updatedBy", AuthenticatedUser.getLoginPersonId());
        query.executeUpdate();
        return updateTimestamp;
    }

    @Override
    public void syncFCOIDisclosure(Integer disclosureId, Integer disclosureNumber) {
        Session session = Objects.requireNonNull(hibernateTemplate.getSessionFactory()).getCurrentSession();
        SessionImpl sessionImpl = (SessionImpl) session;
        Connection connection = sessionImpl.connection();
        try {
            CallableStatement statement = connection.prepareCall("{call COI_SYNC_FCOI_DISCLOSURE(?,?,?)}");
            statement.setInt(1, disclosureId);
            statement.setInt(2, disclosureNumber);
            statement.setString(3, AuthenticatedUser.getLoginPersonId());
            statement.execute();
        } catch (Exception e) {
            e.printStackTrace();
            logger.error("Exception in syncFCOIDisclosure {}", e.getMessage());
//            throw new ApplicationException("Exception in syncFCOIDisclosure ", e, CoreConstants.DB_PROC_ERROR);
        }
    }

    @Override
    public List<COIValidateDto> evaluateValidation(Integer disclosureId, String personId) {
        List<COIValidateDto> coiValidateDtoList = new ArrayList<>();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        SessionImpl sessionImpl = (SessionImpl) session;
        Connection connection = sessionImpl.connection();
        List<COIValidateDataDto> validateDataDtos = new ArrayList<>();
        try {
            CallableStatement statement = connection.prepareCall("{call COI_EVALUATE_VALIDATION(?,?)}");
            statement.setInt(1, disclosureId);
            statement.setString(2, personId);
            statement.execute();
            ResultSet resultSet = statement.getResultSet();
            while (resultSet.next()) {
                COIValidateDataDto validateDataDto = new COIValidateDataDto();
                BeanUtils.populate(validateDataDto, resultSetToMap(resultSet));
                validateDataDtos.add(validateDataDto);
            }
            Map<String, List<COIValidateDataDto>> groupedValidation = validateDataDtos.stream().filter(o -> !o.getVALIDATION_MSG_TYPE()
                            .equals(Constants.COI_VALIDATION_PRO_SFI_ACTION_TYPE))
                    .collect(Collectors.groupingBy(COIValidateDataDto::getMESSAGE, Collectors.toList()));
            groupedValidation.entrySet().forEach(validationObj -> {
                validationObj.getValue().forEach(data -> {
                    COIValidateDto coiValidateDto = new COIValidateDto();
                    coiValidateDto.setValidationMessage(data.getMESSAGE());
                    coiValidateDto.setValidationType(data.getVALIDATION_TYPE());
                    coiValidateDto.setSfiList(data.getSFIs() != null ?
                            Arrays.asList(data.getSFIs().split(":;:")) : new ArrayList<>());
                    coiValidateDtoList.add(coiValidateDto);
                });
            });
            Map<String, List<COIValidateDataDto>> groupedValidationPS = validateDataDtos.stream().filter(o -> o.getVALIDATION_MSG_TYPE().equals(Constants.COI_VALIDATION_PRO_SFI_ACTION_TYPE))
                    .collect(Collectors.groupingBy(COIValidateDataDto::getMESSAGE, Collectors.toList()));
            groupedValidationPS.entrySet().forEach(validationObj -> {
                COIValidateDto coiValidateDto = new COIValidateDto();
                coiValidateDto.setValidationMessage(validationObj.getKey());
                coiValidateDto.setValidationType(validationObj.getValue().get(0).getVALIDATION_TYPE());
                List<List<Map<String, String>>> projectSfiListMaps = validationObj.getValue().stream()
                        .map(item -> Arrays.stream(item.getPROJ_SFI_DETAILS().split("\\|\\|"))
                                .map(part -> Arrays.stream(part.trim().split("::")).map(String::trim).toArray(String[]::new))
                                .collect(Collectors.toMap(pair -> pair[0], pair -> pair[1])))
                        .collect(Collectors.groupingBy(map -> map.get("ModuleItemKey")))
                        .values()
                        .stream()
                        .collect(Collectors.toList());
                coiValidateDto.setProjectSfiList(projectSfiListMaps);
                coiValidateDto.setSfiList(new ArrayList<>());
                coiValidateDtoList.add(coiValidateDto);
            });
            return coiValidateDtoList;
        } catch (Exception e) {
            logger.error("Exception on evaluateValidation {}", e.getMessage());
            throw new ApplicationException("error in evaluateValidation ", e, Constants.DB_PROC_ERROR);
        }
    }

    // Convert ResultSet to Map
    private Map<String, Object> resultSetToMap(ResultSet rs) throws SQLException {
        ResultSetMetaData metaData = rs.getMetaData();
        int numColumns = metaData.getColumnCount();
        Map<String, Object> resultMap = new HashMap<>();
        for (int i = 1; i <= numColumns; i++) {
            String columnName = metaData.getColumnLabel(i);
            Object columnValue = rs.getObject(i);
            resultMap.put(columnName, columnValue);
        }
        return resultMap;
    }

    @Override
    public boolean isProjectSFISyncNeeded(Integer disclosureId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT c.syncNeeded ");
        hqlQuery.append("FROM CoiDisclosure c WHERE  c.disclosureId = :disclosureId ");
        org.hibernate.Query<Boolean> query = session.createQuery(hqlQuery.toString());
        query.setParameter("disclosureId", disclosureId);
        return query.getSingleResult();
    }

    @Override
    public void updateDisclosureSyncNeeded(Integer disclosureId, boolean syncNeeded) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("UPDATE CoiDisclosure c SET c.syncNeeded = :syncNeeded ");
        hqlQuery.append("WHERE  c.disclosureId = :disclosureId ");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("disclosureId", disclosureId);
        query.setParameter("syncNeeded", syncNeeded);
        query.executeUpdate();
    }

    @Override
    public void updateDisclosureSyncNeededByPerEntId(Integer personEntityId, boolean syncNeeded) {
            StringBuilder hqlQuery = new StringBuilder();
            Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
            hqlQuery.append("UPDATE COI_DISCLOSURE d ");
            hqlQuery.append("INNER JOIN PERSON_ENTITY pe ON pe.PERSON_ID = d.PERSON_ID ");
            hqlQuery.append("SET d.SYNC_NEEDED = :syncNeeded ");
            hqlQuery.append("WHERE pe.PERSON_ENTITY_ID = :personEntityId ");
            hqlQuery.append("AND d.REVIEW_STATUS_CODE IN (1, 5, 6) ");
            Query query = session.createNativeQuery(hqlQuery.toString());
            query.setParameter("personEntityId", personEntityId);
            String status = syncNeeded ? "Y" : "N";
            query.setParameter("syncNeeded", status);
            query.executeUpdate();
    }

    @Override
    public void updateFcoiDisclSyncNeedStatus(DisclosureProjectDto projectDto) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        SessionImpl sessionImpl = (SessionImpl) session;
        Connection connection = sessionImpl.connection();
        CallableStatement statement = null;
        try {
            if (oracledb.equalsIgnoreCase("N")) {
                statement = connection.prepareCall("{call COI_SYNC_UPDATE_DISCL_INTEG_UPDATES(?,?)}");
                statement.setInt(1, projectDto.getModuleCode());
                statement.setString(2, projectDto.getProjectId());
            } else if (oracledb.equalsIgnoreCase("Y")) {
                String functionCall = "{call COI_SYNC_UPDATE_DISCL_INTEG_UPDATES(?,?,?)}";
                statement = connection.prepareCall(functionCall);
                statement.registerOutParameter(1, OracleTypes.CURSOR);
                statement.setInt(2, projectDto.getModuleCode());
                statement.setString(3, projectDto.getProjectId());
            }
            Objects.requireNonNull(statement).execute();
        } catch (Exception e) {
            logger.error("Exception on updateFcoiDisclSyncNeedStatus : {} | {} | {}", projectDto.getModuleCode(),projectDto.getProjectId(), e.getMessage());
            throw new ApplicationException("Unable to fetch disclosure", e, Constants.DB_PROC_ERROR);
        }
    }

    @Override
    public DisclComment saveOrUpdateDisclComment(DisclComment disclComment) {
        hibernateTemplate.getSessionFactory().getCurrentSession();
        hibernateTemplate.saveOrUpdate(disclComment);
        hibernateTemplate.flush();
        return disclComment;
    }

    @Override
    public void detachFcoiDisclProject(DisclosureProjectDto projectDto) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        SessionImpl sessionImpl = (SessionImpl) session;
        Connection connection = sessionImpl.connection();
        CallableStatement statement = null;
        try {
            if (oracledb.equalsIgnoreCase("N")) {
                statement = connection.prepareCall("{call COI_SYNC_REMOVE_DEACTIVATED_PROJECTS(?,?)}");
                statement.setInt(1, projectDto.getModuleCode());
                statement.setString(2, projectDto.getProjectNumber());
            } else if (oracledb.equalsIgnoreCase("Y")) {
                String functionCall = "{call COI_SYNC_REMOVE_DEACTIVATED_PROJECTS(?,?,?)}";
                statement = connection.prepareCall(functionCall);
                statement.registerOutParameter(1, OracleTypes.CURSOR);
                statement.setInt(2, projectDto.getModuleCode());
                statement.setString(3, projectDto.getProjectNumber());
            }
            Objects.requireNonNull(statement).execute();
        } catch (Exception e) {
            logger.error("Exception on detachFcoiDisclProject : {} | {} | {}", projectDto.getModuleCode(),projectDto.getProjectNumber(), e.getMessage());
            throw new ApplicationException("Unable to fetch disclosure", e, Constants.DB_PROC_ERROR);
        }
    }
}
