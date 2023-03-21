package com.polus.fibicomp.codetable.dao;

import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.codetable.dto.CodeTableDatabus;
import com.polus.fibicomp.codetable.dto.Fields;
import com.polus.fibicomp.codetable.pojo.CodeTableConfiguration;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dbengine.DBEngine;
import com.polus.fibicomp.dbengine.DBEngineConstants;
import com.polus.fibicomp.dbengine.Parameter;
import com.polus.fibicomp.utils.QueryBuilder;

@Service(value = "codeTableDao")
public class CodeTableDaoImpl implements CodeTableDao {

	protected static Logger logger = LogManager.getLogger(CodeTableDaoImpl.class.getName());

	private static final String INTEGER = "INTEGER";
	private static final String MAX_DATA = "MAX_DATA";
	private static final String UPDATE_USER = "UPDATE_USER";
	private static final String UPDATE_TIMESTAMP = "UPDATE_TIMESTAMP";
	private static final String CONTENT_TYPE = "CONTENT_TYPE";
	private static final String TRUE = "true";

	@Autowired
	private DBEngine dbEngine;

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public String getColumnList(List<Fields> fields, List<String> primaryKeys) {
		String commaSeparatedField = null;
		try {
			List<String> columnList = new ArrayList<>();
			int count = 2;
			for (Fields field : fields) {
				if (Boolean.TRUE.equals(field.getVisible()) && !field.getDataType().equalsIgnoreCase("Blob")
						&& !field.getDataType().equalsIgnoreCase("Clob") && (field.getFilterType() == null || 
								field.getFilterType().equals("systemDropdown") || field.getFilterType().equals("switch")
								|| field.getFilterType().equals("endpoint")) && field.getIsTransient() == null) {
					columnList.add("t1."+field.getColumnName());
				}
				for (String primaryKey : primaryKeys) {
					if (Boolean.FALSE.equals(field.getVisible()) && field.getColumnName().equals(primaryKey)) {
						columnList.add("t1."+primaryKey);
					}
				}
				if (field.getFilterType() != null && field.getFilterType().equals("lookUp")){
					columnList.add("t"+count+"."+field.getColumnName());
					columnList.add("t"+count+".DESCRIPTION AS DESCRIPTION_"+field.getColumnName());
					count ++;
				}
				if (field.getRefColumnName() != null){
					if (field.getColumnName().equals(UPDATE_USER)) {
						columnList.add("t"+count+".FULL_NAME AS "+field.getColumnName());
						count ++;
					} else {
						columnList.add("t"+count+"."+field.getColumnName());
						count ++;
					}
				}
			}
			commaSeparatedField = String.join(",", columnList);
		} catch (Exception e) {
			logger.error("Exception in getColumnList {}", e.getMessage());
		}
		return commaSeparatedField;
	}

	@Override
	public HashMap<String, Object> getChangedValue(MultipartFile[] files, CodeTableDatabus codeTableDatabus) {
		HashMap<String, Object> modifiedData = new HashMap<>();
		try {
			for (Fields field : codeTableDatabus.getCodeTable().getFields()) {
				String changedColumn = null;
				String changedColumnValue = null;
				if (field.getValueChanged() != null && Boolean.TRUE.equals(field.getValueChanged())) {
					changedColumn = field.getColumnName();
				}
				if (changedColumn != null) {
					for (HashMap<String, Object> hmResult : codeTableDatabus.getTableData()) {
						if (hmResult.get(changedColumn) != null) {
							changedColumnValue = hmResult.get(changedColumn).toString();
						}
					}
					if (changedColumnValue != null && changedColumnValue.equalsIgnoreCase("")) {
						changedColumnValue = null;
					}
					modifiedData.put(changedColumn, changedColumnValue);
				}
			}
		} catch (Exception e) {
			logger.error("Exception in getChangedValue {}", e.getMessage());
		}
		return modifiedData;
	}

	@Override
	public HashMap<String, Object> getPrimaryKeyValue(CodeTableDatabus codeTableDatabus) {
		HashMap<String, Object> primaryKeyValues = new HashMap<>();
		try {
			for (String primaryKey : codeTableDatabus.getCodeTable().getPrimaryKey()) {
				Object primaryKeyValue = null;
				for (HashMap<String, Object> hmResult : codeTableDatabus.getPrimaryValues()) {
					if (hmResult.get(primaryKey) != null) {
						primaryKeyValue = hmResult.get(primaryKey);
					}
				}
				primaryKeyValues.put(primaryKey, primaryKeyValue);
			}
		} catch (Exception e) {
			logger.error("Exception in getPrimaryKeyValue {}", e.getMessage());
		}
		return primaryKeyValues;
	}

	@Override
	public ArrayList<Parameter> getInputVariable(List<String> primaryKeyList, HashMap<String, Object> updatedMap, List<Fields> fieldList, HashMap<String, String> changedMap, ArrayList<Parameter> inParam) {
		try {
			for (String primaryKey : primaryKeyList) {
				for (Fields fields : fieldList) {
					if (fields.getColumnName().equalsIgnoreCase(primaryKey)) {
						inParam.add(new Parameter(primaryKey, fields.getDataType(), updatedMap.get(primaryKey)));
					}
				}
			}
		} catch (Exception e) {
			logger.error("Exception in getInputVariable {}", e.getMessage());
		}
		return inParam;
	}

	@Override
	public HashMap<String, Object> generateMandatoryFields(CodeTableDatabus codeTableDatabus, HashMap<String, Object> changedMap, MultipartFile[] files) {
		try {
			for (Fields field : codeTableDatabus.getCodeTable().getFields()) {
				for (String primaryKey : codeTableDatabus.getCodeTable().getPrimaryKey()) {
					if (field.getColumnName().equalsIgnoreCase(primaryKey)) {
						if (Boolean.FALSE.equals(field.getIsEditable())) {
							String query = QueryBuilder.selectMaxEntry(codeTableDatabus.getCodeTable(), primaryKey);
							ArrayList<HashMap<String, Object>> dataList = dbEngine.executeQuerySQL(new ArrayList<>(), query);
							Integer max = 0;
							if (dataList.get(0).get(MAX_DATA) != null) {
								max = Integer.parseInt(dataList.get(0).get(MAX_DATA).toString());
								max++;
							} else {
								max++;
							}
							changedMap.put(primaryKey, max.toString());
						}
					} else if (field.getColumnName().equalsIgnoreCase(CONTENT_TYPE)) {
						changedMap.put(CONTENT_TYPE, files[0].getContentType());
					}
				}
			}
		} catch (Exception e) {
			logger.error("Exception in generatePrimaryKey {}", e.getMessage());
		}
		return changedMap;
	}

	@Override
	public String getAttachmentColumn(List<Fields> fields) {
		String commaSeparatedField = null;
		try {
			List<String> columnList = new ArrayList<>();
			for (Fields field : fields) {
				if (!TRUE.equals(field.getIsTransient())) {
					columnList.add(field.getColumnName());
				}
			}
			commaSeparatedField = String.join(",", columnList);
		} catch (Exception e) {
			logger.error("Exception in getAttachmentColumn {}", e.getMessage());
		}
		return commaSeparatedField;
	}

	@Override
	public String getContentType(ArrayList<HashMap<String, Object>> dataList) {
		String contentType = null;
		try {
			String extentionType = getExtentionType(dataList);
			if (extentionType != null) {
				if (extentionType.equalsIgnoreCase(".jpg")) {
					contentType = "image/jpeg";
				} else if (extentionType.equalsIgnoreCase(".pdf")) {
					contentType = "application/pdf";
				} else if (extentionType.equalsIgnoreCase(".png")) {
					contentType = "image/png";
				} else if (extentionType.equalsIgnoreCase(".txt")) {
					contentType = "text/plain";
				} else if (extentionType.equalsIgnoreCase(".docx")) {
					contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document";
				} else if (extentionType.equalsIgnoreCase(".sql")) {
					contentType = "application/octet-stream";
				} else if (extentionType.equalsIgnoreCase(".eml")) {
					contentType = "message/rfc822";
				} else if (extentionType.equalsIgnoreCase(".xlsx")) {
					contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";
				} else if (extentionType.equalsIgnoreCase(".zip")) {
					contentType = "application/x-zip-compressed";
				} else if (extentionType.equalsIgnoreCase(".doc")) {
					contentType = "application/msword";
				} else if (extentionType.equalsIgnoreCase(".ts")) {
					contentType = "video/vnd.dlna.mpeg-tts";
				} else if (extentionType.equalsIgnoreCase(".ico")) {
					contentType = "image/x-icon";
				} else if (extentionType.equalsIgnoreCase(".sig") || extentionType.equalsIgnoreCase(".json")) {
					contentType = "application/octet-stream";
				} else if (extentionType.equalsIgnoreCase(".js")) {
					contentType = "application/javascript";
				} else if (extentionType.equalsIgnoreCase(".mp3")) {
					contentType = "audio/mp3";
				}
			}
		} catch (Exception e) {
			logger.error("Exception in getContentType {}", e.getMessage());
		}
		return contentType;
	}

	private String getExtentionType(ArrayList<HashMap<String, Object>> dataList) {
		String extentionType = null;
		try {
			if (dataList.get(0).get(CONTENT_TYPE) == null) {
				String filename = dataList.get(0).get("FILE_NAME").toString();
				if (filename.contains(".")) {
					int index = filename.lastIndexOf('.');
					extentionType = filename.substring(index, filename.length());
				}
			} else {
				extentionType = dataList.get(0).get(CONTENT_TYPE).toString();
			}
		} catch (Exception e) {
			logger.error("Exception in getExtentionType {}", e.getMessage());
		}
		return extentionType;
	}

	@Override
	public ArrayList<Parameter> setInsertParamValues(MultipartFile[] files, HashMap<String, Object> changedMap, CodeTableDatabus codeTableDatabus) {
		ArrayList<Parameter> inputParam = new ArrayList<>();
		try {
			for (Fields fields : codeTableDatabus.getCodeTable().getFields()) {
				if (fields.getDataType().equalsIgnoreCase("Blob") || fields.getDataType().equalsIgnoreCase("Clob")) {
					inputParam.add(new Parameter(fields.getColumnName(), fields.getDataType(), files[0].getBytes()));
				} else if (fields.getDataType().equalsIgnoreCase(INTEGER)) {
					inputParam.add(new Parameter(fields.getColumnName(), fields.getDataType(),
							changedMap.get(fields.getColumnName()) == null ? null
									: Integer.parseInt(changedMap.get(fields.getColumnName()).toString())));
				} else if (fields.getDataType().equalsIgnoreCase("Date")) {
					if (!fields.getColumnName().equalsIgnoreCase(UPDATE_TIMESTAMP)) {
						if (changedMap.get(fields.getColumnName()) != null) {
							Timestamp timestamp = new Timestamp (Long.parseLong(changedMap.get(fields.getColumnName()).toString()));
							inputParam.add(new Parameter(fields.getColumnName(), DBEngineConstants.TYPE_TIMESTAMP, timestamp));
						} else {
							inputParam.add(new Parameter(fields.getColumnName(), DBEngineConstants.TYPE_TIMESTAMP, null));
						}
					}
				} else {
					inputParam.add(new Parameter(fields.getColumnName(), fields.getDataType(),
							changedMap.get(fields.getColumnName())));
				}
			}
		} catch (Exception e) {
			logger.error("Exception in setParamValues {}", e.getMessage());
			e.printStackTrace();
		}
		return inputParam;
	}

	private static Timestamp getCurrentTimestamp() {
		return new Timestamp(System.currentTimeMillis());
	}

	@SuppressWarnings("unused")
	private static Timestamp getUpdatedDate(String stringDate) {
		Timestamp timestampDate = null;
		try {
			SimpleDateFormat dateFormat = new SimpleDateFormat(Constants.DEFAULT_DATE_FORMAT);
			java.util.Date date = dateFormat.parse(stringDate);
			timestampDate = new Timestamp(date.getTime());
		} catch (Exception e) {
			logger.error("Exception in getUpdatedDate {}", e.getMessage());
		}
		return timestampDate;
	}

	@Override
	public ArrayList<Parameter> setUpdateParamValues(MultipartFile[] files, HashMap<String, Object> changedMap, HashMap<String, Object> primaryKeyMap, CodeTableDatabus codeTableDatabus) {
		ArrayList<Parameter> inputParam = new ArrayList<>();
		Set<String> changedMapKeySet = new HashSet<>();
		changedMapKeySet.addAll(changedMap.keySet());
		Set<String> primaryKeySet = primaryKeyMap.keySet();
		try {
			for (String key : changedMapKeySet) {
				updateInputParams(files, codeTableDatabus, key, changedMap, inputParam);
			}
			for (String primaryKey : primaryKeySet) {
				updateInputParams(files, codeTableDatabus, primaryKey, primaryKeyMap, inputParam);
			}
		} catch (Exception e) {
			logger.error("Exception in setUpdateParamValues {}", e.getMessage());
		}
		return inputParam;
	}

	private ArrayList<Parameter> updateInputParams(MultipartFile[] files, CodeTableDatabus codeTableDatabus, String key, HashMap<String, Object> changedMap, ArrayList<Parameter> inputParam) throws Exception {
		for (Fields fields : codeTableDatabus.getCodeTable().getFields()) {
			if (fields.getColumnName().equalsIgnoreCase(key)) {
				if (fields.getDataType().equalsIgnoreCase("Blob") || fields.getDataType().equalsIgnoreCase("Clob")) {
					inputParam.add(new Parameter(fields.getColumnName(), fields.getDataType(), files[0].getBytes()));
				} else if (fields.getDataType().equalsIgnoreCase(INTEGER)) {
					inputParam.add(new Parameter(fields.getColumnName(), fields.getDataType(),
							changedMap.get(fields.getColumnName()) != null ? Integer.parseInt(changedMap.get(fields.getColumnName()).toString()) : null));
				} else if (fields.getDataType().equalsIgnoreCase("Date")) {
					if (fields.getColumnName().equalsIgnoreCase(UPDATE_TIMESTAMP)) {
						inputParam.add(new Parameter(fields.getColumnName(), DBEngineConstants.TYPE_TIMESTAMP,
								getCurrentTimestamp()));
					} else {
						Timestamp timestamp = new Timestamp (Long.parseLong(changedMap.get(fields.getColumnName()).toString()));
						inputParam.add(new Parameter(fields.getColumnName(), DBEngineConstants.TYPE_TIMESTAMP, timestamp));
					}
				} else {
					inputParam.add(new Parameter(fields.getColumnName(), fields.getDataType(),
							changedMap.get(fields.getColumnName())));
				}
			}
		}
		return inputParam;
	}

	@Override
	public HashMap<String, Object> getChangedValueForWaf(MultipartFile files, CodeTableDatabus codeTableDatabus) {
		HashMap<String, Object> modifiedData = new HashMap<>();
		try {
			for (Fields field : codeTableDatabus.getCodeTable().getFields()) {
				String changedColumn = null;
				String changedColumnValue = null;
				if (field.getValueChanged() != null && Boolean.TRUE.equals(field.getValueChanged())) {
					changedColumn = field.getColumnName();
				}
				if (changedColumn != null) {
					for (HashMap<String, Object> hmResult : codeTableDatabus.getTableData()) {
						if (hmResult.get(changedColumn) != null) {
							changedColumnValue = hmResult.get(changedColumn).toString();
						}
					}
					if (changedColumnValue != null && changedColumnValue.equalsIgnoreCase("")) {
						changedColumnValue = null;
					}
					modifiedData.put(changedColumn, changedColumnValue);
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Exception in getChangedValue {}", e.getMessage());
		}
		return modifiedData;
	}

	@Override
	public ArrayList<Parameter> setUpdateParamValuesForWaf(MultipartFile files, HashMap<String, Object> changedMap,
			HashMap<String, Object> primaryKeyMap, CodeTableDatabus codeTableDatabus) {
		ArrayList<Parameter> inputParam = new ArrayList<>();
		Set<String> changedMapKeySet = changedMap.keySet();
		Set<String> primaryKeySet = primaryKeyMap.keySet();
		try{
			for(String key : changedMapKeySet){
				updateInputParamsForWaf(files,codeTableDatabus,key,changedMap,inputParam);
			}
			for(String primaryKey : primaryKeySet){
				updateInputParamsForWaf(files,codeTableDatabus,primaryKey,primaryKeyMap,inputParam);
			}
		}catch(Exception e){
			logger.error("Exception in setUpdateParamValues {}", e.getMessage());
		}
		return inputParam;
	}

	private ArrayList<Parameter> updateInputParamsForWaf(MultipartFile files, CodeTableDatabus codeTableDatabus, String key,
			HashMap<String, Object> changedMap, ArrayList<Parameter> inputParam) throws Exception {
		for (Fields fields : codeTableDatabus.getCodeTable().getFields()) {
			if (fields.getColumnName().equalsIgnoreCase(key)) {
				if (fields.getDataType().equalsIgnoreCase("Blob") || fields.getDataType().equalsIgnoreCase("Clob")) {
					inputParam.add(new Parameter(fields.getColumnName(), fields.getDataType(), files.getBytes()));
				} else if (fields.getDataType().equalsIgnoreCase(INTEGER)) {
					inputParam.add(new Parameter(fields.getColumnName(), fields.getDataType(),
							Integer.parseInt(changedMap.get(fields.getColumnName()).toString())));
				} else if (fields.getDataType().equalsIgnoreCase("Date")) {
					if (fields.getColumnName().equalsIgnoreCase(UPDATE_TIMESTAMP)) {
						inputParam.add(new Parameter(fields.getColumnName(), DBEngineConstants.TYPE_TIMESTAMP,
								getCurrentTimestamp()));
					} else {
						Timestamp timestamp = new Timestamp (Long.parseLong(changedMap.get(fields.getColumnName()).toString()));
						inputParam.add(new Parameter(fields.getColumnName(), DBEngineConstants.TYPE_TIMESTAMP, timestamp));
					}
				} else {
					inputParam.add(new Parameter(fields.getColumnName(), fields.getDataType(), changedMap.get(fields.getColumnName())));
				}
			}
		}
		return inputParam;
	}

	@Override
	public ArrayList<Parameter> setInsertParamValuesForWaf(MultipartFile files, HashMap<String, Object> changedMap, CodeTableDatabus codeTableDatabus) {
		ArrayList<Parameter> inputParam = new ArrayList<>();
		try {
			for (Fields fields : codeTableDatabus.getCodeTable().getFields()) {
				if (fields.getDataType().equalsIgnoreCase("Blob") || fields.getDataType().equalsIgnoreCase("Clob")) {
					inputParam.add(new Parameter(fields.getColumnName(), fields.getDataType(), files.getBytes()));
				} else if (fields.getDataType().equalsIgnoreCase(INTEGER)) {
					inputParam.add(new Parameter(fields.getColumnName(), fields.getDataType(),
							Integer.parseInt(changedMap.get(fields.getColumnName()).toString())));
				} else if (fields.getDataType().equalsIgnoreCase("Date")) {
					if (fields.getColumnName().equalsIgnoreCase(UPDATE_TIMESTAMP)) {
						inputParam.add(new Parameter(fields.getColumnName(), DBEngineConstants.TYPE_TIMESTAMP,
								getCurrentTimestamp()));
					} else {
						Timestamp timestamp = new Timestamp (Long.parseLong(changedMap.get(fields.getColumnName()).toString()));
						inputParam.add(new Parameter(fields.getColumnName(), DBEngineConstants.TYPE_TIMESTAMP, timestamp));
					}
				} else {
					inputParam.add(new Parameter(fields.getColumnName(), fields.getDataType(), changedMap.get(fields.getColumnName())));
				}
			}
		} catch(Exception e){
			logger.error("Exception in setParamValues {}", e.getMessage());
			e.printStackTrace();
		}
		return inputParam;
	}

	@Override
	public HashMap<String, Object> generateMandatoryFieldsForWaf(CodeTableDatabus codeTableDatabus, HashMap<String, Object> changedMap, MultipartFile files) {
		try {
			for (Fields field : codeTableDatabus.getCodeTable().getFields()) {
				for (String primaryKey : codeTableDatabus.getCodeTable().getPrimaryKey()) {
					if (field.getColumnName().equalsIgnoreCase(primaryKey)) {
						if (Boolean.FALSE.equals(field.getIsEditable())) {
							String query = QueryBuilder.selectMaxEntry(codeTableDatabus.getCodeTable(), primaryKey);
							ArrayList<HashMap<String, Object>> dataList = dbEngine.executeQuerySQL(new ArrayList<>(), query);
							Integer max = 0;
							if (dataList.get(0).get(MAX_DATA) != null) {
								max = Integer.parseInt(dataList.get(0).get(MAX_DATA).toString());
								max++;
							} else {
								max++;
							}
							changedMap.put(primaryKey, max.toString());
						}
					} else if (field.getColumnName().equalsIgnoreCase(UPDATE_USER)) { 
						changedMap.put(UPDATE_USER, codeTableDatabus.getUpdatedUser());
					} else if (field.getColumnName().equalsIgnoreCase(UPDATE_TIMESTAMP)) {
						changedMap.put(UPDATE_TIMESTAMP, codeTableDatabus.getTableData().get(0).get(UPDATE_TIMESTAMP));
					} else if (field.getColumnName().equalsIgnoreCase(CONTENT_TYPE)) {
						changedMap.put(CONTENT_TYPE, codeTableDatabus.getContentType());
					}
				}
			}
		} catch (Exception e) {
			logger.error("Exception in generatePrimaryKey {}", e.getMessage());			
		}
		return changedMap;
	}

	@Override
	public List<CodeTableConfiguration> loadCodeTableConfiguration() {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<CodeTableConfiguration> criteria = builder.createQuery(CodeTableConfiguration.class);
			Root<CodeTableConfiguration> rootCodeTableConfiguration = criteria.from(CodeTableConfiguration.class);	
			criteria.multiselect(rootCodeTableConfiguration.get("tableName"),rootCodeTableConfiguration.get("displayName"),
					rootCodeTableConfiguration.get("groupName"),rootCodeTableConfiguration.get("description"));
			return session.createQuery(criteria).getResultList();
		});	
	}

	@Override
	public CodeTableConfiguration fetchCodeTableConfigurationByTableName(String tableName) {
		return hibernateTemplate.get(CodeTableConfiguration.class, tableName);
	}	

}
