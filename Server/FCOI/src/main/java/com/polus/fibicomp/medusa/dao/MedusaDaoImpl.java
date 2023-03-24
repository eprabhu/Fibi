package com.polus.fibicomp.medusa.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dbengine.DBEngine;
import com.polus.fibicomp.dbengine.DBEngineConstants;
import com.polus.fibicomp.dbengine.Parameter;
import com.polus.fibicomp.medusa.dto.MedusaDTO;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "medusaDao")
public class MedusaDaoImpl implements MedusaDao {

	protected static Logger logger = LogManager.getLogger(MedusaDaoImpl.class.getName());

	private static final String NEGOTIATION = "NEGO";
	private static final String AWARD = "AWD";
	private static final String DEVELOPMENT_PROPOSAL = "PD";
	private static final String INSTITUTE_PROPOSAL = "IP";
	private static final String GRANT_CALL = "GRANT";
	private static final String AWARD_NUMBER_PARAMETER = "<<AWARD_NUMBER>>";
	private static final String PROPOSAL_ID = "PROPOSAL_ID";
	private static final String PROPOSAL_NUMBER = "PROPOSAL_NUMBER";
	private static final String PROPOSAL_ID_PARAMETER = "<<PROPOSAL_ID>>";
	private static final String AWARD_NUMBER = "AWARD_NUMBER";
	private static final String NEGOTIATION_ID = "NEGOTIATION_ID";
	private static final String NEGOTIATION_ID_PARAMETER = "<<NEGOTIATION_ID>>";
	private static final String TITLE_FIELD = "title";
	private static final String TITLE = "TITLE";
	private static final String LEAD_UNIT_NUMBER_FIELD = "leadUnitNumber";
	private static final String HOME_UNIT_NUMBER = "HOME_UNIT_NUMBER";
	private static final String START_DATE = "START_DATE";
	private static final String END_DATE = "END_DATE";
	private static final String UNIT_NAME_FIELD = "unitName";
	private static final String SPONSOR_FIELD = "sponsor";
	private static final String STATUS_FIELD = "status";
	private static final String START_DATE_FIELD = "startDate";
	private static final String END_DATE_FIELD = "endDate";
	private static final String GRANT_HEADER_ID = "GRANT_HEADER_ID";
	private static final String ASSOCIATED_PROJECT_ID = "ASSOCIATED_PROJECT_ID";

	private static final String PROPOSAL_NUMBER_PARAMETER = "<<PROPOSAL_NUMBER>>";

	@Autowired
	public DBEngine dbEngine;

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	private CommonService commonService;

	@Override
	public MedusaDTO getMedusa(String projectId, Integer moduleCode) {
		MedusaDTO medusa = new MedusaDTO();
		if (moduleCode.equals(Constants.NEGOTIATION_MODULE_CODE)) {
			medusa.setModuleName(NEGOTIATION);
			medusa.setProjectNumber(projectId);
			medusa.setModuleCode(Constants.NEGOTIATION_MODULE_CODE);
			String associationType = getAssociationType(Integer.parseInt(projectId));
			if (Constants.switchToAward.equals(associationType)) {
				medusa = getAwardFromNego(medusa, Integer.parseInt(projectId));
			} else if (Constants.switchToIP.equals(associationType)) {
				medusa = getIPFromNego(medusa, Integer.parseInt(projectId));
			}
		} else if (moduleCode.equals(Constants.AWARD_MODULE_CODE)) {
			medusa.setModuleName(AWARD);
			medusa.setProjectNumber(projectId);
			medusa.setModuleCode(Constants.AWARD_MODULE_CODE);
			MedusaDTO medusaIP = getIPFromAward(projectId);
			medusa.setMedusa(medusaIP.getMedusa());
		} else if (moduleCode.equals(Constants.INSTITUTE_PROPOSAL_MODULE_CODE)) {
			List<MedusaDTO> medusas = new ArrayList<>();
			medusa.setModuleName(INSTITUTE_PROPOSAL);
			medusa.setProjectNumber(projectId);
			medusa.setModuleCode(Constants.INSTITUTE_PROPOSAL_MODULE_CODE);
			MedusaDTO medusaAward = getAwardFromIP(projectId);
			if (medusaAward != null && medusaAward.getMedusa() != null && !medusaAward.getMedusa().isEmpty()) {
				medusas.addAll(medusaAward.getMedusa());
			}
			MedusaDTO medusaDto = new MedusaDTO();
			medusaDto = getPDFromIP(medusaDto, projectId);
			if (medusaDto != null && medusaDto.getMedusa() != null && !medusaDto.getMedusa().isEmpty()) {
				medusas.addAll(medusaDto.getMedusa());
			}
			medusa.setMedusa(medusas);
		} else if (moduleCode.equals(Constants.DEV_PROPOSAL_MODULE_CODE)) {
			medusa.setModuleName(DEVELOPMENT_PROPOSAL);
			medusa.setProjectNumber(projectId);
			medusa.setModuleCode(Constants.DEV_PROPOSAL_MODULE_CODE);
			getIPFromPD(medusa, projectId);
		}
		return medusa;
	}

	public MedusaDTO getIPFromAward(String awardNumber) {
		MedusaDTO ipMedusa = new MedusaDTO();
		ArrayList<Parameter> inParam = new ArrayList<>();
		List<MedusaDTO> medusas = new ArrayList<>();
		inParam.add(new Parameter(AWARD_NUMBER_PARAMETER, DBEngineConstants.TYPE_STRING, awardNumber));
		try {
			List<HashMap<String, Object>> output = dbEngine.executeQuery(inParam, "GET_MEDUSA_IP_FROM_AWARD");
			if (output != null && !output.isEmpty()) {
				for (HashMap<String, Object> hmOutput : output) {
					MedusaDTO medusaDto = new MedusaDTO();
					String ipNumber = hmOutput.get(PROPOSAL_NUMBER).toString();
					if (ipNumber != null) {
						medusaDto.setModuleName(INSTITUTE_PROPOSAL);
						medusaDto.setProjectNumber(hmOutput.get(PROPOSAL_NUMBER).toString());
						medusaDto.setModuleCode(Constants.INSTITUTE_PROPOSAL_MODULE_CODE);
						medusaDto = getPDFromIP(medusaDto, ipNumber);
						List<MedusaDTO> ipMedusas = medusaDto.getMedusa();
						MedusaDTO negotiationMedusa = getNegoFromIP(ipNumber);
						if (negotiationMedusa != null) {
							ipMedusas.add(negotiationMedusa);
						}
						medusaDto.setMedusa(ipMedusas);
						medusas.add(medusaDto);
					}
				}
				ipMedusa.setMedusa(medusas);
			}
		} catch (Exception e) {
			logger.error("Exception in getIPFromAward :{}", e.getMessage());
		}
		return ipMedusa;
	}

	public MedusaDTO getIPFromAward(MedusaDTO medusa, String awardNumber) {
		ArrayList<Parameter> inParam = new ArrayList<>();
		List<MedusaDTO> medusas = new ArrayList<>();
		inParam.add(new Parameter(AWARD_NUMBER_PARAMETER, DBEngineConstants.TYPE_STRING, awardNumber));
		try {
			List<HashMap<String, Object>> output = dbEngine.executeQuery(inParam, "GET_MEDUSA_IP_FROM_AWARD");
			if (output != null && !output.isEmpty()) {
				for (HashMap<String, Object> hmOutput : output) {
					MedusaDTO ipMedusa = new MedusaDTO();
					if (hmOutput.get(PROPOSAL_ID) != null) {
						ipMedusa.setModuleName(INSTITUTE_PROPOSAL);
						ipMedusa.setProjectNumber(hmOutput.get(PROPOSAL_NUMBER).toString());
						ipMedusa.setModuleCode(Constants.INSTITUTE_PROPOSAL_MODULE_CODE);
						ipMedusa = getPDFromIP(ipMedusa, hmOutput.get(PROPOSAL_ID).toString());
						medusas.add(ipMedusa);
					}
				}
			}
			medusa.setMedusa(medusas);
		} catch (Exception e) {
			logger.error("Exception in getIPFromAward : {}", e.getMessage());
		}
		return medusa;
	}

	public MedusaDTO getPDFromIP(MedusaDTO medusa, String projectId) {
		ArrayList<Parameter> inParam = new ArrayList<>();
		List<MedusaDTO> medusas = new ArrayList<>();
		inParam.add(new Parameter(PROPOSAL_NUMBER_PARAMETER, DBEngineConstants.TYPE_STRING, projectId));
		try {
			List<HashMap<String, Object>> output = dbEngine.executeQuery(inParam, "GET_MEDUSA_PD_FROM_IP");
			if (output != null && !output.isEmpty()) {
				for (HashMap<String, Object> hmOutput : output) {
					MedusaDTO proposalMedusa = new MedusaDTO();
					if (hmOutput.get("DEV_PROPOSAL_NUMBER") != null) {
						String proposalNumber = hmOutput.get("DEV_PROPOSAL_NUMBER").toString();
						proposalMedusa.setModuleName(DEVELOPMENT_PROPOSAL);
						proposalMedusa.setProjectNumber(proposalNumber);
						proposalMedusa = getGrantFromPD(proposalMedusa, proposalNumber);
						proposalMedusa.setModuleCode(Constants.DEV_PROPOSAL_MODULE_CODE);
						medusas.add(proposalMedusa);
					}
				}
			}
			medusa.setMedusa(medusas);
		} catch (Exception e) {
			logger.error("Exception in getPDFromIP : {}", e.getMessage());
		}
		return medusa;
	}

	public MedusaDTO getGrantFromPD(MedusaDTO medusa, String projectId) {
		ArrayList<Parameter> inParam = new ArrayList<>();
		List<MedusaDTO> medusas = new ArrayList<>();
		inParam.add(new Parameter(PROPOSAL_ID_PARAMETER, DBEngineConstants.TYPE_STRING, projectId));
		try {
			List<HashMap<String, Object>> output = dbEngine.executeQuery(inParam, "GET_MEDUSA_GRANT_FROM_PD");
			if (output != null && !output.isEmpty()) {
				for (HashMap<String, Object> hmOutput : output) {
					MedusaDTO grantMedusa = new MedusaDTO();
					if (hmOutput.get(GRANT_HEADER_ID) != null) {
						grantMedusa.setModuleName(GRANT_CALL);
						grantMedusa.setProjectNumber(hmOutput.get(GRANT_HEADER_ID).toString());
						grantMedusa.setModuleCode(Constants.GRANTCALL_MODULE_CODE);
						medusas.add(grantMedusa);
					}
				}
			}
			medusa.setMedusa(medusas);
		} catch (Exception e) {
			logger.error("Exception in getGrantFromPD : {}", e.getMessage());
		}
		return medusa;
	}

	private MedusaDTO getIPFromPD(MedusaDTO medusa, String projectId) {
		ArrayList<Parameter> inParam = new ArrayList<>();
		List<MedusaDTO> medusas = new ArrayList<>();
		inParam.add(new Parameter(PROPOSAL_ID_PARAMETER, DBEngineConstants.TYPE_STRING, projectId));
		try {
			List<HashMap<String, Object>> output = dbEngine.executeQuery(inParam, "GET_MEDUSA_IP_FROM_PD");
			if (output != null && !output.isEmpty()) {
				for (HashMap<String, Object> hmOutput : output) {
					MedusaDTO medusaDto = new MedusaDTO();
					List<MedusaDTO> ipMedusas = new ArrayList<>();
					if (hmOutput.get(PROPOSAL_NUMBER) != null) {
						String iPNumber = hmOutput.get(PROPOSAL_NUMBER).toString();
						medusaDto.setModuleName(INSTITUTE_PROPOSAL);
						medusaDto.setProjectNumber(hmOutput.get(PROPOSAL_NUMBER).toString());
						medusaDto.setModuleCode(Constants.INSTITUTE_PROPOSAL_MODULE_CODE);
						MedusaDTO ipMedusa = getNegoFromIP(iPNumber);
						if (ipMedusa != null && ipMedusa.getProjectNumber() != null) {
							ipMedusas.add(ipMedusa);
						}
						MedusaDTO awardMedusa = new MedusaDTO();
						awardMedusa = getAwardFromIP(awardMedusa, iPNumber);
						if (awardMedusa != null && awardMedusa.getProjectNumber() != null) {
							ipMedusas.add(awardMedusa);
						}
						medusaDto.setMedusa(ipMedusas);
						medusas.add(medusaDto);
					}
				}
				medusa.setMedusa(medusas);
			}
		} catch (Exception e) {
			logger.error("Exception in getNegoFromAward : {}", e.getMessage());
		}
		return medusa;
	}

	private MedusaDTO getNegoFromIP(String projectId) {
		MedusaDTO negotiationMedusa = null;
		ArrayList<Parameter> inParam = new ArrayList<>();
		inParam.add(new Parameter("<<ASSOCIATED_PROJECT_ID>>", DBEngineConstants.TYPE_STRING, projectId));
		try {
			List<HashMap<String, Object>> output = dbEngine.executeQuery(inParam, "GET_MEDUSA_NEGO_FROM_IP");
			if (output != null && !output.isEmpty()) {
				for (HashMap<String, Object> hmOutput : output) {
					negotiationMedusa = new MedusaDTO();
					if (hmOutput.get(NEGOTIATION_ID) != null) {
						negotiationMedusa.setModuleName(NEGOTIATION);
						negotiationMedusa.setProjectNumber(hmOutput.get(NEGOTIATION_ID).toString());
						negotiationMedusa.setModuleCode(Constants.NEGOTIATION_MODULE_CODE);
					}
				}
			}
		} catch (Exception e) {
			logger.error("Exception in getNegoFromAward :{}", e.getMessage());
		}
		return negotiationMedusa;
	}

	private MedusaDTO getNegoFromAward(MedusaDTO medusa, String projectId) {
		ArrayList<Parameter> inParam = new ArrayList<>();
		List<MedusaDTO> negotiationMedusas = new ArrayList<>();
		inParam.add(new Parameter("<<ASSOCIATED_PROJECT_ID>>", DBEngineConstants.TYPE_STRING, projectId));
		try {
			List<HashMap<String, Object>> output = dbEngine.executeQuery(inParam, "GET_MEDUSA_NEGO_FROM_AWARD");
			if (output != null && !output.isEmpty()) {
				for (HashMap<String, Object> hmOutput : output) {
					MedusaDTO negotiationMedusa = new MedusaDTO();
					if (hmOutput.get(NEGOTIATION_ID) != null) {
						String negotiationId = hmOutput.get(NEGOTIATION_ID).toString();
						negotiationMedusa.setModuleName(NEGOTIATION);
						negotiationMedusa.setProjectNumber(negotiationId);
						negotiationMedusa.setModuleCode(Constants.NEGOTIATION_MODULE_CODE);
						negotiationMedusas.add(negotiationMedusa);
					}
				}
			}
			medusa.setMedusa(negotiationMedusas);
		} catch (Exception e) {
			logger.error("Exception in getNegoFromAward :{}", e.getMessage());
		}
		return medusa;
	}

	@Override
	public HashMap<String, Object> getProjectDetailsSP(String moduleKey, String moduleNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer code = 0;
		if (moduleKey != null) {
			if (moduleKey.equalsIgnoreCase(AWARD)) {
				code = 1;
			} else if (moduleKey.equalsIgnoreCase(INSTITUTE_PROPOSAL)) {
				code = 2;
			} else if (moduleKey.equalsIgnoreCase(DEVELOPMENT_PROPOSAL)) {
				code = 3;
			} else if (moduleKey.equalsIgnoreCase(NEGOTIATION)) {
				code = 5;
			} else if (moduleKey.equalsIgnoreCase(GRANT_CALL)) {
				code = 15;
			}
		}
		HashMap<String, Object> detailsField = new HashMap<>();
		try {
			if (oracledb.equalsIgnoreCase(Constants.dbOracle)) {
				String procedureName = "GET_MORE_PROJECT_DETAILS";
				String functionCall = "{call " + procedureName + "(?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.setInt(1, code);
				statement.setString(2, moduleNumber);
				statement.registerOutParameter(3, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(3);
			} else if (oracledb.equalsIgnoreCase(Constants.dbMySQL)) {
				statement = connection.prepareCall("{call GET_MORE_PROJECT_DETAILS(?,?)}");
				statement.setInt(1, code);
				statement.setString(2, moduleNumber);
				statement.execute();
				resultSet = statement.getResultSet();
			}
			if (resultSet != null) {
				while (resultSet.next()) {
					if (code.equals(Constants.AWARD_MEDUSA)) {
						detailsField.put("award_id", resultSet.getString("AWARD_ID"));
						detailsField.put("awardNumber", resultSet.getString(AWARD_NUMBER));
						detailsField.put("accountNumber", resultSet.getString("ACCOUNT_NUMBER"));
						detailsField.put("awardEffectiveDate", resultSet.getTimestamp("AWARD_EFFECTIVE_DATE"));
						detailsField.put("finalExpirationDate", resultSet.getTimestamp("FINAL_EXPIRATION_DATE"));
						detailsField.put("awardStatus", resultSet.getString("AWARD_STATUS"));
						detailsField.put("piName", resultSet.getString("PI_NAME"));
						detailsField.put("leadUnitName", resultSet.getString("LEAD_UNIT_NAME"));
						detailsField.put("sponsorName", commonService.getSponsorFormatBySponsorDetail(resultSet.getString("SPONSOR_CODE"), resultSet.getString("SPONSOR_NAME"), resultSet.getString("ACRONYM")));
						detailsField.put(LEAD_UNIT_NUMBER_FIELD, resultSet.getString("LEAD_UNIT_NUMBER"));
						detailsField.put(TITLE_FIELD, resultSet.getString(TITLE));
						detailsField.put("obligatedStartDate", resultSet.getTimestamp("OBLIGATED_START"));
						detailsField.put("obligatedEndDate", resultSet.getTimestamp("OBLIGATED_END"));
						detailsField.put("anticipatedTotalAmount", resultSet.getString("ANTICIPATED_TOTAL_AMOUNT"));
						detailsField.put("sponsorAwardNumber", resultSet.getString("SPONSOR_AWARD_NUMBER"));
						detailsField.put("activityTypeCode", resultSet.getString("ACTIVITY_TYPE_CODE"));
						detailsField.put("beginDate", resultSet.getTimestamp("AWARD_BEGIN_DATE"));
					} else if (code.equals(Constants.INSTITUTEPROPOSAL_MEDUSA)) {
						detailsField.put("proposalNumber", resultSet.getString(PROPOSAL_NUMBER));
						detailsField.put("proposalId", resultSet.getString(PROPOSAL_ID));
						detailsField.put(TITLE_FIELD, resultSet.getString(TITLE));
						detailsField.put(LEAD_UNIT_NUMBER_FIELD, resultSet.getString(HOME_UNIT_NUMBER));
						detailsField.put(UNIT_NAME_FIELD, resultSet.getString("UNIT_NAME"));
						detailsField.put("activityType", resultSet.getString("ACTIVITY_TYPE"));
						detailsField.put("investigator", resultSet.getString("INVESTIGATOR"));
						detailsField.put(SPONSOR_FIELD, commonService.getSponsorFormatBySponsorDetail(resultSet.getString("SPONSOR_CODE"), resultSet.getString("SPONSOR"), resultSet.getString("ACRONYM")));
						detailsField.put("proposalType", resultSet.getString("PROPOSAL_TYPE"));
						detailsField.put(STATUS_FIELD, resultSet.getString("STATUS"));
						detailsField.put(START_DATE_FIELD, resultSet.getTimestamp(START_DATE));
						detailsField.put(END_DATE_FIELD, resultSet.getTimestamp(END_DATE));
					} else if (code.equals(Constants.PROPOSALDEVELOPMENT_MEDUSA)) {
						detailsField.put(LEAD_UNIT_NUMBER_FIELD, resultSet.getString(HOME_UNIT_NUMBER));
						detailsField.put(TITLE_FIELD, resultSet.getString(TITLE));
						detailsField.put(UNIT_NAME_FIELD, resultSet.getString("HOME_UNIT_NAME"));
						detailsField.put("activityType", resultSet.getString("ACTIVITY_TYPE_DESCRIPTION"));
						detailsField.put(SPONSOR_FIELD, commonService.getSponsorFormatBySponsorDetail(resultSet.getString("SPONSOR_CODE"), resultSet.getString("SPONSOR_NAME"), resultSet.getString("ACRONYM")));
						detailsField.put(STATUS_FIELD, resultSet.getString("STATUS_DESCRIPTION"));
						detailsField.put("investigator", resultSet.getString("FULL_NAME"));
						detailsField.put("proposalType", resultSet.getString("TYPE_DESCRIPTION"));
						detailsField.put("abstractDescription", resultSet.getString("ABSTRACT_DESC"));
						detailsField.put("ipNumber", resultSet.getString("IP_NUMBER"));
						detailsField.put(START_DATE_FIELD, resultSet.getTimestamp(START_DATE));
						detailsField.put(END_DATE_FIELD, resultSet.getTimestamp(END_DATE));
					} else if (code.equals(Constants.NEGOTIATION_MEDUSA)) {
						detailsField.put("negotiationId", resultSet.getString(NEGOTIATION_ID));
						detailsField.put("negotiationStatusCode", resultSet.getString("NEGOTIATION_STATUS_CODE"));
						detailsField.put("negotiationStatus", resultSet.getString("NEGOTIATION_STATUS"));
						detailsField.put("agreementTypeCode", resultSet.getString("AGREEMENT_TYPE_CODE"));
						detailsField.put("agreementType", resultSet.getString("AGREEMENT_TYPE"));
						detailsField.put("negotiatorPersonId", resultSet.getString("NEGOTIATOR_PERSON_ID"));
						detailsField.put("negotiatorFullName", resultSet.getString("NEGOTIATOR_FULL_NAME"));
						detailsField.put(START_DATE_FIELD, resultSet.getTimestamp(START_DATE));
						detailsField.put(END_DATE_FIELD, resultSet.getTimestamp(END_DATE));
						detailsField.put("updateTimeStamp", resultSet.getTimestamp("UPDATE_TIMESTAMP"));
						detailsField.put("updateUser", resultSet.getString("UPDATE_USER"));
						detailsField.put("createUser", resultSet.getString("CREATE_USER"));
						detailsField.put("createTimeStamp", resultSet.getTimestamp("CREATE_TIMESTAMP"));
						detailsField.put("summaryComment", resultSet.getString("SUMMARY_COMMENT"));
						detailsField.put("negotiatorComment", resultSet.getString("NEGOTIATOR_COMMENT"));
						detailsField.put("legalComment", resultSet.getString("LEGAL_COMMENT"));
					} else if (code.equals(Constants.GRANTCALL_MEDUSA)) {
						detailsField.put("grantCallId", resultSet.getString(GRANT_HEADER_ID));
						detailsField.put("grantType", resultSet.getString("GRANT_TYPE_DESC"));
						detailsField.put(STATUS_FIELD, resultSet.getString("GRANT_STATUS_DESC"));
						detailsField.put(START_DATE_FIELD, resultSet.getTimestamp("OPENING_DATE"));
						detailsField.put(END_DATE_FIELD, resultSet.getTimestamp("CLOSING_DATE"));
						detailsField.put(TITLE_FIELD, resultSet.getString("NAME"));
						detailsField.put("description", resultSet.getString("DESCRIPTION"));
						detailsField.put("maximumBudget", resultSet.getString("MAX_BUDGET"));
						detailsField.put("sponsorCode", resultSet.getString("SPONSOR_CODE"));
						detailsField.put(SPONSOR_FIELD, resultSet.getString("SPONSOR_TYPE_DESC"));
						detailsField.put("fundingSource", resultSet.getString("FUNDING_SOURCE_DESC"));
						detailsField.put("grantTheme", resultSet.getString("GRANT_THEME"));
						detailsField.put(UNIT_NAME_FIELD, resultSet.getString("HOME_UNIT_NAME"));
						detailsField.put(LEAD_UNIT_NUMBER_FIELD, resultSet.getString(HOME_UNIT_NUMBER));
						detailsField.put("applicationProcedure", resultSet.getString("APPLICATION_PROCEDURE"));
					}
				}
			}
		} catch (Exception e) {
			logger.error("Exception in getProjectDetailsSP : {}", e.getMessage());
		}
		return detailsField;
	}

	public MedusaDTO getAwardFromNego(MedusaDTO medusa, Integer projectId) {
		List<MedusaDTO> awardMedusas = new ArrayList<>();
		ArrayList<Parameter> inParam = new ArrayList<>();
		inParam.add(new Parameter(NEGOTIATION_ID_PARAMETER, DBEngineConstants.TYPE_INTEGER, projectId));
		try {
			List<HashMap<String, Object>> output = dbEngine.executeQuery(inParam, "GET_MEDUSA_AWARD_FROM_NEGO");
			if (output != null && !output.isEmpty()) {
				for (HashMap<String, Object> hmOutput : output) {
					MedusaDTO awardMedusa = new MedusaDTO();
					if (hmOutput.get(ASSOCIATED_PROJECT_ID) != null) {
						String awardNumber = hmOutput.get(ASSOCIATED_PROJECT_ID).toString();
						awardMedusa.setModuleName(AWARD);
						awardMedusa.setProjectNumber(awardNumber);
						awardMedusa.setModuleCode(Constants.AWARD_MODULE_CODE);
						awardMedusa = getIPFromAward(awardMedusa, awardNumber);
						awardMedusas.add(awardMedusa);
					}
				}
				medusa.setMedusa(awardMedusas);
			}
		} catch (Exception e) {
			logger.error("Exception in getAwardFromNego : {}", e.getMessage());
		}
		return medusa;
	}

	public MedusaDTO getIPFromNego(MedusaDTO medusa, Integer projectId) {
		List<MedusaDTO> ipMedusas = new ArrayList<>();
		ArrayList<Parameter> inParam = new ArrayList<>();
		inParam.add(new Parameter(NEGOTIATION_ID_PARAMETER, DBEngineConstants.TYPE_INTEGER, projectId));
		try {
			List<HashMap<String, Object>> output = dbEngine.executeQuery(inParam, "GET_MEDUSA_IP_FROM_NEGO");
			if (output != null && !output.isEmpty()) {
				for (HashMap<String, Object> hmOutput : output) {
					MedusaDTO ipMedusa = new MedusaDTO();
					if (hmOutput.get(ASSOCIATED_PROJECT_ID) != null) {
						String ipNumber = hmOutput.get(ASSOCIATED_PROJECT_ID).toString();
						String proposalId = hmOutput.get(PROPOSAL_NUMBER).toString();
						ipMedusa.setModuleName(INSTITUTE_PROPOSAL);
						ipMedusa.setProjectNumber(ipNumber);
						ipMedusa.setModuleCode(Constants.INSTITUTE_PROPOSAL_MODULE_CODE);
						MedusaDTO medusaAwardDto = getAwardFromIP(ipMedusa, proposalId);
						MedusaDTO medusaPDDto = new MedusaDTO();
						medusaPDDto = getPDFromIP(medusaPDDto, proposalId);
						ipMedusa.setMedusa(medusaPDDto.getMedusa());
						ipMedusas.add(ipMedusa);
						if (medusaAwardDto != null && medusaAwardDto.getMedusa() != null
								&& !medusaAwardDto.getMedusa().isEmpty()) {
							ipMedusas.addAll(medusaAwardDto.getMedusa());
						}
					}
				}
				medusa.setMedusa(ipMedusas);
			}
		} catch (Exception e) {
			logger.error("Exception in getIPFromNego : {}", e.getMessage());
		}
		return medusa;
	}

	private String getAssociationType(Integer projectId) {
		List<HashMap<String, Object>> alAssociationType = isNegotiationAssociation(projectId);
		if (alAssociationType != null && !alAssociationType.isEmpty()) {
			HashMap<String, Object> hmAssociationType = alAssociationType.get(0);
			return (String) hmAssociationType.get("ASSOCIATION_TYPE_CODE");
		}
		return null;
	}

	public List<HashMap<String, Object>> isNegotiationAssociation(Integer negotiationId) {
		List<HashMap<String, Object>> output = new ArrayList<>();
		ArrayList<Parameter> inParam = new ArrayList<>();
		inParam.add(new Parameter(NEGOTIATION_ID_PARAMETER, DBEngineConstants.TYPE_INTEGER, negotiationId));
		try {
			output = dbEngine.executeQuery(inParam, "GET_NEGO_ASSOCIATION");
		} catch (Exception e) {
			logger.error("Exception in isNegotiationAssociation : {}", e.getMessage());
		}
		return output;
	}

	public String getIPNumber(String proposalId) {
		ArrayList<Parameter> inParam = new ArrayList<>();
		inParam.add(new Parameter(PROPOSAL_ID_PARAMETER, DBEngineConstants.TYPE_STRING, proposalId));
		try {
			List<HashMap<String, Object>> output = dbEngine.executeQuery(inParam, "GET_MEDUSA_IP_NUMBER_IP_ID");
			if (output != null && !output.isEmpty()) {
				HashMap<String, Object> hmIPNumber = output.get(0);
				return (String) hmIPNumber.get(PROPOSAL_NUMBER);
			}
		} catch (Exception e) {
			logger.error("Exception in isNegotiationAssociation : {}", e.getMessage());
		}
		return null;
	}

	public MedusaDTO getAwardFromIP(MedusaDTO medusa, String projectId) {
		MedusaDTO awardMedusa = null;
		List<MedusaDTO> awardMedusas = new ArrayList<>();
		ArrayList<Parameter> inParam = new ArrayList<>();
		inParam.add(new Parameter(PROPOSAL_NUMBER_PARAMETER, DBEngineConstants.TYPE_STRING, projectId));
		try {
			List<HashMap<String, Object>> output = dbEngine.executeQuery(inParam, "GET_MEDUSA_AWARD_FROM_IP");
			if (output != null && !output.isEmpty()) {
				for (HashMap<String, Object> hmOutput : output) {
					awardMedusa = new MedusaDTO();
					if (hmOutput.get(AWARD_NUMBER) != null) {
						String awardNumber = hmOutput.get(AWARD_NUMBER).toString();
						awardMedusa.setModuleName(AWARD);
						awardMedusa.setProjectNumber(awardNumber);
						awardMedusa.setModuleCode(Constants.AWARD_MODULE_CODE);
						getNegoFromAward(awardMedusa, awardNumber);
						awardMedusas.add(awardMedusa);
					}
				}
				medusa.setMedusa(awardMedusas);
			}
		} catch (Exception e) {
			logger.error("Exception in getAwardFromNego : {}", e.getMessage());
		}
		return awardMedusa;
	}

	public MedusaDTO getAwardFromIP(String projectId) {
		MedusaDTO ipMedusa = new MedusaDTO();
		List<MedusaDTO> medusas = new ArrayList<>();
		ArrayList<Parameter> inParam = new ArrayList<>();
		inParam.add(new Parameter(PROPOSAL_NUMBER_PARAMETER, DBEngineConstants.TYPE_STRING, projectId));
		try {
			List<HashMap<String, Object>> output = dbEngine.executeQuery(inParam, "GET_MEDUSA_AWARD_FROM_IP");
			if (output != null && !output.isEmpty()) {
				for (HashMap<String, Object> hmOutput : output) {
					MedusaDTO awardMedusa = new MedusaDTO();
					if (hmOutput.get(AWARD_NUMBER) != null) {
						awardMedusa.setModuleName(AWARD);
						awardMedusa.setProjectNumber(hmOutput.get(AWARD_NUMBER).toString());
						awardMedusa = getNegoFromAward(awardMedusa, hmOutput.get(AWARD_NUMBER).toString());
						awardMedusa.setModuleCode(Constants.AWARD_MODULE_CODE);
						List<MedusaDTO> negotiationMedusas = awardMedusa.getMedusa();
						MedusaDTO negotiationMedusa = getNegoFromIP(projectId);
						if (negotiationMedusa != null) {
							negotiationMedusas.add(negotiationMedusa);
						}
						awardMedusa.setMedusa(negotiationMedusas);
						medusas.add(awardMedusa);
					}
				}
				ipMedusa.setMedusa(medusas);
			}
		} catch (Exception e) {
			logger.error("Exception in getAwardFromIP : {}", e.getMessage());
		}
		return ipMedusa;
	}

	public ArrayList getServiceRequestDetail(int moduleCode, String moduleItemKey,int subModuleCode, String subModuleItemKey, String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		ArrayList serviceRequestData = new ArrayList<>();
		try {
			if (oracledb.equalsIgnoreCase(Constants.dbOracle)) {
				statement = connection.prepareCall("{call GET_SERVICE_REQUEST_DETAILS_MEDUSA(?,?,?,?,?,?)}");
				statement.setInt(1, moduleCode);
				statement.setString(2, moduleItemKey);
				statement.setInt(3,subModuleCode);
				statement.setString(4,subModuleItemKey);
				statement.setString(5,personId);
				statement.registerOutParameter(6, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(6);
			} else if (oracledb.equalsIgnoreCase(Constants.dbMySQL)) {
				statement = connection.prepareCall("{call GET_SERVICE_REQUEST_DETAILS_MEDUSA(?,?,?,?,?)}");
				statement.setInt(1, moduleCode);
				statement.setString(2, moduleItemKey);
				statement.setInt(3,subModuleCode);
				statement.setString(4,subModuleItemKey);
				statement.setString(5,personId);
				statement.execute();
				resultSet = statement.getResultSet();
			}
			if(resultSet != null) {
				while (resultSet.next()) {
					HashMap<String, Object> detailsField = new HashMap<>();
					detailsField.put("serviceRequestId", resultSet.getString("SR_HEADER_ID"));
					detailsField.put("subject", resultSet.getString("SUBJECT"));
					detailsField.put("status", resultSet.getString("STATUS"));
					detailsField.put("type", resultSet.getString("TYPE"));
					detailsField.put("unitNumber", resultSet.getString("UNIT_NUMBER"));
					detailsField.put("unitName", resultSet.getString("UNIT_NAME"));
					detailsField.put("reportedPersonId", resultSet.getString("REPORTER_PERSON_ID"));
					detailsField.put("reportedBy", resultSet.getString("REPORTED_BY"));
					detailsField.put("reportedOn", resultSet.getString("CREATE_TIMESTAMP"));
					serviceRequestData.add(detailsField);
				}
			}
		} catch (Exception e) {
			logger.error("Exception in getServiceRequestDetail : {}", e.getMessage());
		}
		return serviceRequestData;
	}
}
