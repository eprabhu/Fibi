package com.polus.fibicomp.mapmaintenance.dao;

import java.util.ArrayList;
import java.util.HashMap;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.dbengine.DBEngine;
import com.polus.fibicomp.dbengine.DBEngineConstants;
import com.polus.fibicomp.dbengine.Parameter;

@Service
public class MapMaintenanceDao {
	protected static Logger logger = LogManager.getLogger(MapMaintenanceDao.class.getName());

	@Autowired
	private DBEngine dbEngine;

	@Autowired
	private CommonDao commonDao;

	/* Automatic Increment for MAP_ID */
	public synchronized Integer getNextMapId() {
		Integer columnId = null;
		try {
			columnId = getMapMaxColumnValue("get_max_wrklfw_map_map_id", "MAP_ID");
			updateMapMaxColumnValue("update_max_wrklfw_map_map_id", "MAP_ID", columnId);
			return columnId;
		} catch (Exception e) {
			logger.error("Exception in getQuestionnaireId" + e.getMessage());
			return 0;
		}
	}

	private Integer getMapMaxColumnValue(String SqlId, String columnName) {
		Integer columnId = null;
		try {
			ArrayList<Parameter> inputParam = new ArrayList<>();
			ArrayList<HashMap<String, Object>> result = dbEngine.executeQuery(inputParam, SqlId);
			HashMap<String, Object> hmResult = result.get(0);
			if (hmResult.get(columnName) == null) {
				return 0;
			} else {
				columnId = Integer.parseInt(hmResult.get(columnName).toString());
			}
			return ++columnId;
		} catch (Exception e) {
			logger.error("Exception in getMaxColumnValue" + e.getMessage());
			return null;
		}
	}

	private void updateMapMaxColumnValue(String SqlId, String columnName, Integer maxColumnValue) {
		try {
			ArrayList<Parameter> inputParam = new ArrayList<>();
			inputParam.add(new Parameter(columnName, DBEngineConstants.TYPE_INTEGER, maxColumnValue));
			dbEngine.executeUpdate(inputParam, SqlId);
		} catch (Exception e) {
			logger.error("Exception in updateMaxColumnValue" + e.getMessage());
		}
	}

	/* Automatic Increment for MAP_DETAIL_ID */
	public synchronized Integer getNextMapDetailId() {
		Integer columnId = null;
		try {
			columnId = getMaxMapDetailColumnValue("get_max_wrklfw_map_detail_map_id", "MAP_DETAIL_ID");
			updateMaxMapDetailColumnValue("update_max_wrklfw_map_detail_map_id", "MAP_DETAIL_ID", columnId);
			return columnId;
		} catch (Exception e) {
			logger.error("Exception in getQuestionnaireId" + e.getMessage());
			return 0;
		}
	}

	private Integer getMaxMapDetailColumnValue(String SqlId, String columnName) {
		Integer columnId = null;
		try {
			ArrayList<Parameter> inputParam = new ArrayList<>();
			ArrayList<HashMap<String, Object>> result = dbEngine.executeQuery(inputParam, SqlId);
			HashMap<String, Object> hmResult = result.get(0);
			if (hmResult.get(columnName) == null) {
				return 0;
			} else {
				columnId = Integer.parseInt(hmResult.get(columnName).toString());
			}
			return ++columnId;
		} catch (Exception e) {
			logger.error("Exception in getMaxColumnValue" + e.getMessage());
			return null;
		}
	}

	private void updateMaxMapDetailColumnValue(String SqlId, String columnName, Integer maxColumnValue) {
		try {
			ArrayList<Parameter> inputParam = new ArrayList<>();
			inputParam.add(new Parameter(columnName, DBEngineConstants.TYPE_INTEGER, maxColumnValue));
			dbEngine.executeUpdate(inputParam, SqlId);
		} catch (Exception e) {
			logger.error("Exception in updateMaxColumnValue" + e.getMessage());
		}
	}

	/* Fetch All Map Details */
	public ArrayList<HashMap<String, Object>> getMapList() {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			output = dbEngine.executeQuery(inParam, "GET_WORKFLOW_MAP");
			for (HashMap<String, Object> hmResult : output ) {
				hmResult.put("MAP_NAME", hmResult.get("MAP_NAME") == null ? "" : hmResult.get("MAP_NAME"));
				hmResult.put("DESCRIPTION", hmResult.get("DESCRIPTION") == null ? "" : hmResult.get("DESCRIPTION"));
				hmResult.put("UNIT_NAME", hmResult.get("UNIT_NAME") == null ? "" : hmResult.get("UNIT_NAME"));
			}
		} catch (Exception e) {
			logger.error("Exception in getWorkflowMap " + e.getMessage());

		}

		return output;
	}

	/* Fetch Details of Role description */
	public ArrayList<HashMap<String, Object>> getRoleDescription() {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			output = dbEngine.executeQuery(inParam, "GET_ROLE_DESCRIPTION");
		} catch (Exception e) {
			logger.error("Exception in getruleDescriptionMap " + e.getMessage());
		}
		return output;
	}

	/* Fetch Maps based on Id */
	public ArrayList<HashMap<String, Object>> getMapById(int mapId) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<MAP_ID>>", DBEngineConstants.TYPE_INTEGER, mapId));
			output = dbEngine.executeQuery(inParam, "get_map_by_id");
		} catch (Exception e) {
			logger.error("Exception in getMap " + e.getMessage());
		}
		return output;
	}

	/* Fetch Details of Maps based on Id */
	public ArrayList<HashMap<String, Object>> getMapDetailsById(int mapId) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();

			inParam.add(new Parameter("<<MAP_ID>>", DBEngineConstants.TYPE_INTEGER, mapId));
			output = dbEngine.executeQuery(inParam, "GET_MAP_DETAILS_BY_ID");
			for (HashMap<String, Object> param : output) {
				if (param.get("APPROVER_NAME") == null) {
					param.put("APPROVER_NAME", param.get("FULL_NAME"));
				}
			}
		} catch (Exception e) {
			logger.error("Exception in getMapDetails " + e.getMessage());
		}
		return output;
	}

	/* Inactivate entire Map based on Id */
	public void deleteMap(int mapId) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<MAP_ID>>", DBEngineConstants.TYPE_INTEGER, mapId));
			Integer isUpdate = dbEngine.executeUpdate(inParam, "DELETE_MAP_DETAILS");
			if (isUpdate != 0) {
				dbEngine.executeUpdate(inParam, "DELETE_MAP");
			}
		} catch (Exception e) {
			logger.error("Exception in deleteMap " + e.getMessage());
		}
	}

	/* Inactivate entire Map details based on Id */
	public void deleteMapDetailsById(int mapDetailId) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<MAP_DETAIL_ID>>", DBEngineConstants.TYPE_INTEGER, mapDetailId));
			dbEngine.executeUpdate(inParam, "delete_map_detail_by_id");

		} catch (Exception e) {
			logger.error("Exception in deleteMap " + e.getMessage());
		}

	}

	/* Insertion to table WORKFLOW_MAP */
	public int insertMap(HashMap<String, Object> hmMaps, int mapId) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<MAP_ID>>", DBEngineConstants.TYPE_INTEGER, mapId));
			inParam.add(new Parameter("<<DESCRIPTION>>", DBEngineConstants.TYPE_STRING, hmMaps.get("DESCRIPTION")));
			inParam.add(new Parameter("<<MAP_TYPE>>", DBEngineConstants.TYPE_STRING, hmMaps.get("MAP_TYPE")));
			inParam.add(new Parameter("<<UPDATE_TIMESTAMP>>", DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter("<<UNIT_NUMBER>>", DBEngineConstants.TYPE_STRING, hmMaps.get("UNIT_NUMBER")));
			inParam.add(new Parameter("<<UPDATE_USER>>", DBEngineConstants.TYPE_STRING, hmMaps.get("UPDATE_USER")));
			inParam.add(new Parameter("<<MAP_NAME>>", DBEngineConstants.TYPE_STRING, hmMaps.get("MAP_NAME")));
			dbEngine.executeUpdate(inParam, "INSERT_MAP");
			return 1;
		} catch (Exception e) {
			logger.error("Exception in insertMap" + e.getMessage());
			return 0;
		}
	}

	/* Insertion to table WORKFLOW_MAP_DETAILS */
	public void insertMapDetail(HashMap<String, Object> hmMaps, int mapId) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<MAP_DETAIL_ID>>", DBEngineConstants.TYPE_INTEGER, getNextMapDetailId()));
			inParam.add(new Parameter("<<MAP_ID>>", DBEngineConstants.TYPE_INTEGER, mapId));
			inParam.add(new Parameter("<<APPROVAL_STOP_NUMBER>>", DBEngineConstants.TYPE_INTEGER,
					hmMaps.get("APPROVAL_STOP_NUMBER")));
			inParam.add(new Parameter("<<APPROVER_NUMBER>>", DBEngineConstants.TYPE_INTEGER,
					hmMaps.get("APPROVER_NUMBER")));
			inParam.add(new Parameter("<<APPROVER_PERSON_ID>>", DBEngineConstants.TYPE_STRING,
					hmMaps.get("APPROVER_PERSON_ID")));
			inParam.add(new Parameter("<<PRIMARY_APPROVER_FLAG>>", DBEngineConstants.TYPE_STRING,
					hmMaps.get("PRIMARY_APPROVER_FLAG")));
			inParam.add(new Parameter("<<IS_ROLE>>", DBEngineConstants.TYPE_STRING, hmMaps.get("IS_ROLE")));
			inParam.add(
					new Parameter("<<ROLE_TYPE_CODE>>", DBEngineConstants.TYPE_INTEGER, hmMaps.get("ROLE_TYPE_CODE")));
			inParam.add(new Parameter("<<DESCRIPTION>>", DBEngineConstants.TYPE_STRING, hmMaps.get("DESCRIPTION")));
			inParam.add(new Parameter("<<UPDATE_TIMESTAMP>>", DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter("<<UPDATE_USER>>", DBEngineConstants.TYPE_STRING, hmMaps.get("UPDATE_USER")));
			inParam.add(new Parameter("<<STOP_NAME>>", DBEngineConstants.TYPE_STRING, hmMaps.get("STOP_NAME")));
			dbEngine.executeUpdate(inParam, "INSERT_MAP_DETAIL");
		} catch (Exception e) {
			logger.error("Exception in insertMap" + e.getMessage());
		}
	}

	/* Updation to table WORKFLOW_MAP */
	public int updateMap(HashMap<String, Object> listMap) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<DESCRIPTION>>", DBEngineConstants.TYPE_STRING, listMap.get("DESCRIPTION")));
			inParam.add(new Parameter("<<MAP_TYPE>>", DBEngineConstants.TYPE_STRING, listMap.get("MAP_TYPE")));
			inParam.add(new Parameter("<<UPDATE_TIMESTAMP>>", DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter("<<UPDATE_USER>>", DBEngineConstants.TYPE_STRING, listMap.get("UPDATE_USER")));
			inParam.add(new Parameter("<<UNIT_NUMBER>>", DBEngineConstants.TYPE_STRING, listMap.get("UNIT_NUMBER")));
			inParam.add(new Parameter("<<MAP_NAME>>", DBEngineConstants.TYPE_STRING, listMap.get("MAP_NAME")));
			inParam.add(new Parameter("<<MAP_ID>>", DBEngineConstants.TYPE_INTEGER, listMap.get("MAP_ID")));
			dbEngine.executeUpdate(inParam, "update_map_by_id");
			return 1;
		} catch (Exception e) {
			System.out.println(e);
			logger.info("Exception in update" + e);
			return 0;
		}
	}

	/* Updation to table WORKFLOW_MAP_DETAILS */
	public void updateMapDetail(HashMap<String, Object> detailListMap, int mapId) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<APPROVAL_STOP_NUMBER>>", DBEngineConstants.TYPE_INTEGER,
					detailListMap.get("APPROVAL_STOP_NUMBER")));
			inParam.add(new Parameter("<<APPROVER_NUMBER>>", DBEngineConstants.TYPE_INTEGER,
					detailListMap.get("APPROVER_NUMBER")));
			inParam.add(new Parameter("<<APPROVER_PERSON_ID>>", DBEngineConstants.TYPE_STRING,
					detailListMap.get("APPROVER_PERSON_ID")));
			inParam.add(new Parameter("<<PRIMARY_APPROVER_FLAG>>", DBEngineConstants.TYPE_STRING,
					detailListMap.get("PRIMARY_APPROVER_FLAG")));
			inParam.add(new Parameter("<<IS_ROLE>>", DBEngineConstants.TYPE_STRING, detailListMap.get("IS_ROLE")));
			inParam.add(new Parameter("<<ROLE_TYPE_CODE>>", DBEngineConstants.TYPE_INTEGER,
					detailListMap.get("ROLE_TYPE_CODE")));
			inParam.add(
					new Parameter("<<DESCRIPTION>>", DBEngineConstants.TYPE_STRING, detailListMap.get("DESCRIPTION")));
			inParam.add(new Parameter("<<UPDATE_TIMESTAMP>>", DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(
					new Parameter("<<UPDATE_USER>>", DBEngineConstants.TYPE_STRING, detailListMap.get("UPDATE_USER")));
			inParam.add(new Parameter("<<STOP_NAME>>", DBEngineConstants.TYPE_STRING, detailListMap.get("STOP_NAME")));
			inParam.add(new Parameter("<<MAP_DETAIL_ID>>", DBEngineConstants.TYPE_INTEGER,
					detailListMap.get("MAP_DETAIL_ID")));
			inParam.add(new Parameter("<<MAP_ID>>", DBEngineConstants.TYPE_INTEGER, mapId));
			dbEngine.executeUpdate(inParam, "update_map_detail_by_id");
		} catch (Exception e) {
			logger.error("Exception in insertMap" + e.getMessage());
		}
	}

	public ArrayList<HashMap<String, Object>> getUnitList() {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			output = dbEngine.executeQuery(inParam, "GET_ALL_UNITS");
		} catch (Exception e) {
			logger.error("Exception in getUnitList " + e.getMessage());
		}
		return output;

	}

	public Integer isMapUsed(int mapId) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		Integer count = 0;
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<MAP_ID>>", DBEngineConstants.TYPE_INTEGER, mapId));
			inParam.add(new Parameter("<<MAP_ID>>", DBEngineConstants.TYPE_INTEGER, mapId));
			inParam.add(new Parameter("<<MAP_ID>>", DBEngineConstants.TYPE_INTEGER, mapId));
			inParam.add(new Parameter("<<MAP_ID>>", DBEngineConstants.TYPE_INTEGER, mapId));
			output = dbEngine.executeQuery(inParam, "GET_IS_MAP_USED");
			for (HashMap<String, Object> hmRules : output) {
				count = Integer.parseInt(hmRules.get("COUNT").toString());
			}
		} catch (Exception e) {
			logger.error("Exception in map detail check " + e.getMessage());
		}
		return count;
	}

	public ArrayList<HashMap<String, Object>> getRoleDescriptionByModuleCode(Integer moduleCode, Integer subModuleCode) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<MODULE_CODE>>", DBEngineConstants.TYPE_INTEGER, moduleCode));
			inParam.add(new Parameter("<<SUB_MODULE_CODE>>", DBEngineConstants.TYPE_INTEGER, subModuleCode));
			output = dbEngine.executeQuery(inParam, "GET_ROLE_DESCRIPTION_BY_MODULE_CODE");
		} catch (Exception e) {
			logger.error("Exception in getMapDetails " + e.getMessage());
		}
		return output;
	}
}
