package com.polus.fibicomp.fcoiDisclosure.dao;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.type.CollectionType;
import com.polus.core.applicationexception.dto.ApplicationException;
import com.polus.core.common.dao.CommonDao;
import com.polus.core.security.AuthenticatedUser;
import com.polus.fibicomp.coi.dto.CoiConflictStatusTypeDto;
import com.polus.fibicomp.coi.dto.CoiDisclEntProjDetailsDto;
import com.polus.fibicomp.coi.dto.CoiDisclosureDto;
import com.polus.fibicomp.coi.dto.CoiEntityDto;
import com.polus.fibicomp.coi.dto.DisclosureProjectDto;
import com.polus.fibicomp.coi.pojo.CoiConflictHistory;
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
import oracle.jdbc.OracleTypes;
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
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

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
    public CoiDisclosure loadDisclosure (Integer disclosureId) {
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
        hqlQuery.append("d.updateUser = :updateUser where d.disclosureId = :disclosureId");
        Timestamp updateTimestamp = commonDao.getCurrentTimestamp();
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("disclosureId", coiDisclosureDto.getDisclosureId());
        query.setParameter("riskCategoryCode", coiDisclosureDto.getRiskCategoryCode());
        query.setParameter("updateTimestamp", updateTimestamp);
        query.setParameter("updateUser", AuthenticatedUser.getLoginUserName());
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
        query.setParameter("disclosureId",disclosureId);
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
                        .moduleCode(rset.getInt("MODULE_CODE"))
                        .projectId(rset.getString("PROJECT_ID"))
                        .projectNumber(rset.getString("PROJECT_NUMBER"))
                        .title(rset.getString("TITLE"))
                        .projectStatus(rset.getString("STATUS"))
                        .projectStartDate(rset.getTimestamp("START_DATE"))
                        .projectEndDate(rset.getTimestamp("END_DATE"))
                        .homeUnitNumber(rset.getString("HOME_UNIT_NUMBER"))
                        .homeUnitName(rset.getString("HOME_UNIT_NAME"))
                        .sponsorName(rset.getString("SPONSOR_NAME"))
                        .primeSponsorName(rset.getString("PRIME_SPONSOR_NAME"))
                        .piName(rset.getString("PI"))
                        .keyPersonId(rset.getString("KEY_PERSON_ID"))
                        .keyPersonName(rset.getString("KEY_PERSON"))
                        .reporterRole(rset.getString("REPORTER_ROLE"))
                        .conflictStatus(rset.getString("CONFLICT_DESCRIPTION"))
                        .conflictStatusCode(rset.getString("PROJECT_CONFLICT_STATUS_CODE"))
                        .relationShipExists(rset.getBoolean("RELATIONSHIP_EXISTS"))
                        .sfiCompleted(rset.getBoolean("IS_PROJECT_COMPLETED"))
                        .disclosureStatusCount(rset.getString("CONFLICT_STATUS_COUNT") != null ? convertJsonStringToListMap(rset.getString("CONFLICT_STATUS_COUNT")) : null)
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
        }catch (Exception e) {
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
        if(session.createQuery(query).getSingleResult() != null) {
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
        criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginPersonId());
        criteriaUpdate.set("expirationDate",coiDisclosure.getExpirationDate());
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
            statement.setString(3, AuthenticatedUser.getLoginUserName());
            statement.execute();
            ResultSet rset = statement.getResultSet();
            if (rset != null && rset.next()) {
                CoiConflictStatusTypeDto  conflictStatusTypeDto = new CoiConflictStatusTypeDto();
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
        hqlQuery.append("SELECT ch.conflictStatusCode FROM CoiConflictHistory ch ");
        hqlQuery.append("WHERE ch.coiDisclProjectEntityRelId = :coiDisclProjectEntityRelId ORDER BY ch.updateTimestamp DESC LIMIT 1");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("coiDisclProjectEntityRelId", coiDisclProjectEntityRelId);
        Object resultData = query.getSingleResult();
        return resultData != null ? (String) resultData : "";
    }

    @Override
    public void saveOrUpdateCoiConflictHistory(CoiConflictHistory coiConflictHistory) {
        hibernateTemplate.saveOrUpdate(coiConflictHistory);
//        return coiConflictHistory;
    }

    @Override
    public void saveOrUpdateCoiDisclEntProjDetails(CoiDisclProjectEntityRel entityProjectRelation) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("UPDATE CoiDisclProjectEntityRel r SET r.projectConflictStatusCode = :projectConflictStatusCode, ");
        hqlQuery.append("r.updatedBy = :updatedBy, r.updateTimestamp = :updateTimestamp, r.prePersonEntityId = :prePersonEntityId ");
        hqlQuery.append("WHERE r.coiDisclProjectEntityRelId = :coiDisclProjectEntityRelId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("coiDisclProjectEntityRelId", entityProjectRelation.getCoiDisclProjectEntityRelId());
        query.setParameter("projectConflictStatusCode", entityProjectRelation.getProjectConflictStatusCode());
        query.setParameter("prePersonEntityId", entityProjectRelation.getPrePersonEntityId());
        query.setParameter("updatedBy", AuthenticatedUser.getLoginPersonId());
        query.setParameter("updateTimestamp", commonDao.getCurrentTimestamp());
        query.executeUpdate();
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
            Long  count = (Long ) countData;
            return  count.intValue() != 0 ? Boolean.FALSE : Boolean.TRUE;
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
        hqlQuery.append("AND r.coiDisclProject.moduleCode = :moduleCode ");
        hqlQuery.append("AND r.coiDisclProject.moduleItemKey = :moduleItemKey ");
        Query query = session.createQuery(hqlQuery.toString(), Long.class);
        query.setParameter("disclosureId", disclosureId);
        query.setParameter("moduleCode", moduleCode);
        query.setParameter("moduleItemKey", moduleItemId);
        Object countData = query.getSingleResult();
        if (countData != null) {
            Long  count = (Long ) countData;
            return  count.intValue() != 0 ? Boolean.FALSE : Boolean.TRUE;
        }
        return null;
    }

    @Override
    public Timestamp updateDisclosureUpdateDetails(Integer disclosureId) {
        Timestamp updateTimestamp = commonDao.getCurrentTimestamp();
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("UPDATE CoiDisclosure cd SET cd.updateTimestamp = :updateTimestamp, cd.updateUser = :updateUser where cd.disclosureId = :disclosureId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("disclosureId", disclosureId);
        query.setParameter("updateTimestamp", updateTimestamp);
        query.setParameter("updateUser", AuthenticatedUser.getLoginUserName());
        query.executeUpdate();
        return updateTimestamp;
    }

    @Override
    public List<CoiDisclEntProjDetailsDto> getDisclEntProjDetails(ConflictOfInterestVO vo) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        List<CoiDisclEntProjDetailsDto> disclosureProjects = new ArrayList<>();
        SessionImpl sessionImpl = (SessionImpl) session;
        Connection connection = sessionImpl.connection();
        CallableStatement statement;
        ResultSet rset = null;
        try {
            if (oracledb.equalsIgnoreCase("N")) {
                statement = connection.prepareCall("{call COI_DISCL_ENT_PROJ_DETAILS(?, ?, ?, ?)}");
                statement.setInt(1, vo.getDisclosureId());
                if (vo.getPersonEntityId() == null) {
                    statement.setNull(2, Types.INTEGER);
                }
                else {
                    statement.setInt(2, vo.getPersonEntityId());
                }
                if (vo.getModuleCode() == null) {
                    statement.setNull(3, Types.INTEGER);
                }
                else {
                    statement.setInt(3, vo.getModuleCode());
                }
                if (vo.getModuleItemId() == null) {
                    statement.setNull(4, Types.VARCHAR);
                }
                else {
                    statement.setInt(4, vo.getModuleItemId());
                }
                statement.execute();
                rset = statement.getResultSet();
            } else if (oracledb.equalsIgnoreCase("Y")) {
                String procedureName = "COI_DISCL_ENT_PROJ_DETAILS";
                String functionCall = "{call " + procedureName + "(?,?,?,?,?)}";
                statement = connection.prepareCall(functionCall);
                statement.registerOutParameter(1, OracleTypes.CURSOR);
                statement.setNull(2, vo.getDisclosureId());
                if (vo.getPersonEntity() == null) {
                    statement.setNull(3, Types.INTEGER);
                }
                else {
                    statement.setInt(3, vo.getPersonEntityId());
                }
                if (vo.getModuleCode() == null) {
                    statement.setNull(4, Types.INTEGER);
                }
                else {
                    statement.setInt(4, vo.getModuleCode());
                }
                if (vo.getModuleItemId() == null) {
                    statement.setNull(5, Types.INTEGER);
                }
                else {
                    statement.setInt(5, vo.getModuleItemId());
                }
                statement.execute();
                rset = (ResultSet) statement.getObject(1);
            }
            while (rset != null && rset.next()) {
                CoiDisclEntProjDetailsDto entProjDetailsDto = new CoiDisclEntProjDetailsDto();
                entProjDetailsDto.setCoiDisclProjectEntityRelId(rset.getInt("COI_DISCL_PROJECT_ENTITY_REL_ID"));
                entProjDetailsDto.setDisclosureId(rset.getInt("DISCLOSURE_ID"));
                entProjDetailsDto.setDisclosureNumber(rset.getInt("DISCLOSURE_NUMBER"));
                entProjDetailsDto.setPersonEntityId(rset.getInt("PERSON_ENTITY_ID"));
                entProjDetailsDto.setPersonEntityNumber(rset.getInt("PERSON_ENTITY_NUMBER"));
                entProjDetailsDto.setPrePersonEntityId(rset.getInt("PREVIOUS_PERSON_ENTITY_ID"));
                entProjDetailsDto.setEntityId(rset.getInt("ENTITY_ID"));
//                entProjDetailsDto.setEntityNumber(rset.getInt("ENTITY_NUMBER"));
                entProjDetailsDto.setModuleCode(rset.getInt("MODULE_CODE"));
                entProjDetailsDto.setModuleItemKey(rset.getString("MODULE_ITEM_KEY"));
                entProjDetailsDto.setProjectConflictStatusCode(rset.getString("PROJECT_CONFLICT_STATUS_CODE"));
                entProjDetailsDto.setUpdatedBy(rset.getString("UPDATED_BY"));
                entProjDetailsDto.setUpdateTimestamp(rset.getTimestamp("UPDATE_TIMESTAMP"));
                entProjDetailsDto.setProjectId(rset.getString("PROJECT_ID"));
                entProjDetailsDto.setProjectNumber(rset.getString("PROJECT_NUMBER"));
                entProjDetailsDto.setProjectTitle(rset.getString("PROJECT_TITLE"));
                entProjDetailsDto.setProjectTypeCode(rset.getString("COI_PROJECT_TYPE_CODE"));
                entProjDetailsDto.setProjectType(rset.getString("COI_PROJECT_TYPE"));
                entProjDetailsDto.setProjectBadgeColour(rset.getString("BADGE_COLOR"));
                CoiProjConflictStatusType coiProjConflictStatusType = new CoiProjConflictStatusType();
                coiProjConflictStatusType.setDescription(rset.getString("PROJECT_CONFLICT_STATUS"));
                coiProjConflictStatusType.setProjectConflictStatusCode(rset.getString("PROJECT_CONFLICT_STATUS_CODE"));
                entProjDetailsDto.setCoiProjConflictStatusType(coiProjConflictStatusType);
                CoiEntityDto coiEntityDto = new CoiEntityDto();
                coiEntityDto.setEntityId(rset.getInt("ENTITY_ID"));
                coiEntityDto.setEntityNumber(rset.getInt("ENTITY_NUMBER"));
                coiEntityDto.setEntityName(rset.getString("ENTITY_NAME"));
                coiEntityDto.setVersionNumber(rset.getInt("VERSION_NUMBER"));
                coiEntityDto.setVersionStatus(rset.getString("VERSION_STATUS"));
                coiEntityDto.setEntityStatusCode(rset.getString("ENTITY_STATUS_CODE"));
                coiEntityDto.setCreateUser(rset.getString("ENTITY_CREATE_USER"));
                coiEntityDto.setCreateTimestamp(rset.getTimestamp("ENTITY_CREATE_TIMESTAMP"));
                coiEntityDto.setUpdateUser(rset.getString("ENTITY_UPDATE_USER"));
                coiEntityDto.setUpdateTimestamp(rset.getTimestamp("ENTITY_UPDATE_TIMESTAMP"));
                coiEntityDto.setRiskCategoryCode(rset.getString("RISK_CATEGORY_CODE"));
                coiEntityDto.setCountryName(rset.getString("ENTITY_COUNTRY_NAME"));
                coiEntityDto.setEntityType(rset.getString("ENTITY_TYPE"));
                entProjDetailsDto.setCoiEntity(coiEntityDto);
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
    public boolean evaluateDisclosureQuestionnaire(Integer moduleCode,Integer submoduleCode,Integer moduleItemKey) {
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
            statement.setInt(4, moduleItemKey);
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
    public Integer getNumberOfSFIBasedOnDisclosureId(Integer disclosureId) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        StringBuilder hqlQuery = new StringBuilder();
        hqlQuery.append("SELECT COUNT(DISTINCT r.personEntityId) FROM CoiDisclProjectEntityRel r ");
        hqlQuery.append("WHERE r.coiDisclProject.disclosureId = :disclosureId ");
        Query query = session.createQuery(hqlQuery.toString(), Long.class);
        query.setParameter("disclosureId", disclosureId);
        Object countData = query.getSingleResult();
        if (countData != null) {
            return (Integer ) countData;
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
                }
                else {
                    statement.setInt(2, moduleCode);
                }
                if (moduleItemKey == null) {
                    statement.setNull(3, Types.VARCHAR);
                }
                else {
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
                mapObj.put("projectDisclosure", rset.getInt("projectDisclosure") == 0 ? null : rset.getInt("pendingDisclosure"));
                mapObj.put("fcoiDisclosure", rset.getInt("fcoiDisclosure") == 0 ? null : rset.getInt("fcoiDisclosure"));
            }
        } catch (Exception e) {
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
                statement = connection.prepareCall("{call COI_DISCL_PROJ_INSERTION(?,?,?,?)}");
                statement.setInt(1, disclosureId);
                statement.setInt(2, disclosureNumber);
                statement.setString(3, loginPersonId);
                statement.setString(4, loginPersonId);
                statement.execute();
                rset = statement.getResultSet();
            } else if (oracledb.equalsIgnoreCase("Y")) {
                String functionCall = "{call COI_DISCL_PROJ_INSERTION(?,?,?,?,?)}";
                statement = connection.prepareCall(functionCall);
                statement.registerOutParameter(1, OracleTypes.CURSOR);
                statement.setInt(2, disclosureId);
                statement.setInt(3, disclosureNumber);
                statement.setString(4, loginPersonId);
                statement.setString(5, loginPersonId);
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
            logger.error("Exception on syncFcoiDisclosureProjects {}", e.getMessage());
//            throw new ApplicationException("Unable to fetch disclosure", e, Constants.DB_PROC_ERROR);
        }
        return null;
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
    public void syncFcoiDisclProjectsAndEntities(Integer disclosureId, Integer disclosureNumber,Integer coiDisclProjectId, Integer moduleCode,
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
            }

            Objects.requireNonNull(statement).execute();
//            connection.close();
        } catch (Exception e) {
            logger.error("Exception on syncFcoiDisclProjectsAndEntities for coiDisclProjectId : {} | {}",coiDisclProjectId,  e.getMessage());
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
        return (boolean)query.getSingleResult();
    }

    @Override
    public boolean isSameAdminPersonOrGroupAdded(Integer adminGroupId, String adminPersonId, Integer disclosureId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT case when (count(c.disclosureId) > 0) then true else false end ");
        hqlQuery.append("FROM CoiDisclosure c WHERE  c.adminPersonId = :adminPersonId ");
        if (adminGroupId != null)
            hqlQuery.append("AND c.adminGroupId = :adminGroupId ") ;
        hqlQuery.append("AND c.disclosureId = : disclosureId");
        Query query = session.createQuery(hqlQuery.toString());
        if (adminGroupId != null)
            query.setParameter("adminGroupId", adminGroupId);
        query.setParameter("adminPersonId", adminPersonId);
        query.setParameter("disclosureId", disclosureId);
        return (boolean)query.getSingleResult();
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
            logger.error("Exception in syncFCOIDisclosure {}", e.getMessage());
        }
    }
}
