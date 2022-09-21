package com.polus.fibicomp.codetable.service;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.codetable.dao.CodeTableDao;
import com.polus.fibicomp.codetable.dao.JSONParser;
import com.polus.fibicomp.codetable.dto.CodeTable;
import com.polus.fibicomp.codetable.dto.CodeTableDatabus;
import com.polus.fibicomp.codetable.dto.Fields;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.dbengine.DBEngine;
import com.polus.fibicomp.dbengine.Parameter;
import com.polus.fibicomp.utils.QueryBuilder;

@Service(value = "codeTableSerivce")
public class CodeTableSerivceImpl implements CodeTableSerivce {

	protected static Logger logger = LogManager.getLogger(CodeTableSerivceImpl.class.getName());

	private static final String ERROR_FOUND = "Error found";

	@Autowired
	private DBEngine dbEngine;

	@Autowired
	private CodeTableDao codeTableDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private CommonService commonService;

	@Value("${oracledb}")
	private String oracledb;

	@Override
	public CodeTableDatabus getCodeTableDetail(HttpServletRequest request, CodeTableDatabus codeTableDatabus)
			throws Exception {
		try {
			codeTableDatabus.setConfigFile(JSONParser.getJSONData());
			List<Fields> fields = new ArrayList<>();
			Fields field = new Fields();
			fields.add(field);
			CodeTable codeTable = new CodeTable();
			codeTable.setFields(fields);
			codeTableDatabus.setCodeTable(codeTable);
		} catch (Exception e) {
			codeTableDatabus.setPromptCode(0);
			codeTableDatabus.setPromptMessage(e.getMessage());
			logger.error("Exception in getCodeTableDetail {} ", e.getMessage());
		}
		return codeTableDatabus;
	}

	@Override
	public CodeTableDatabus getTableDetail(CodeTableDatabus codeTableDatabus) throws Exception {
		if (codeTableDatabus != null) {
			fetchCodeTableObject(codeTableDatabus, false);
			try {
				String columnList = codeTableDao.getColumnList(codeTableDatabus.getCodeTable().getFields(), codeTableDatabus.getCodeTable().getPrimaryKey());
				String joinQuery = createJoinQuery(codeTableDatabus.getCodeTable().getFields(),
						codeTableDatabus.getCodeTable().getPrimaryKey());
				String finalQuery = null;
				if (joinQuery != null) {
					finalQuery = new StringBuilder().append(codeTableDatabus.getCodeTable().getDatabaseTableName())
							.append(" t1 ").append(joinQuery).toString();
				} else {
					finalQuery = codeTableDatabus.getCodeTable().getDatabaseTableName();
				}
				ArrayList<HashMap<String, Object>> dataList = dbEngine.executeQuerySQL(new ArrayList<>(), QueryBuilder.selectQuery(finalQuery, columnList));
				codeTableDatabus.setTableData(dataList);
			} catch (Exception e) {
				codeTableDatabus.setPromptCode(0);
				codeTableDatabus.setPromptMessage(e.getMessage());
				logger.error("Exception in getTableDetail {} ", e.getMessage());
			}
		}
		return codeTableDatabus;
	}

	private String createJoinQuery(List<Fields> fields, List<String> keys) {
		StringBuilder query = new StringBuilder("");
		int count = 2;
		Boolean isGroupByRequired = false;
		for (Fields field : fields) {
			String tableNumber = "t" + count + "";
			if (field.getFilterType() != null && field.getFilterType().equals("lookUp")) {
				query.append(" LEFT OUTER join "
						+ field.getValueField().substring(0, field.getValueField().indexOf("#")) + " " + tableNumber
						+ " on t1." + field.getColumnName() + "= " + tableNumber + "." + field.getColumnName() + " ");
				count++;
				isGroupByRequired = true;
			}
			if (field.getRefColumnName() != null){
				String tableName = field.getRefColumnName().substring(0, field.getRefColumnName().indexOf("#"));
				String columnName = field.getRefColumnName().substring(field.getRefColumnName().indexOf("#")+1, field.getRefColumnName().length());
				if (field.getColumnName().equals("UPDATE_USER")) {
					query.append(" LEFT OUTER join "
							+ tableName + " " + tableNumber
							+ " on t1.UPDATE_USER = " + tableNumber + "." + columnName + " ");
					count++;
				} else {
					query.append(" LEFT OUTER join "
							+ tableName + " " + tableNumber
							+ " on t1." + columnName + "= " + tableNumber + "." + columnName + " ");
					count++;
				}
			}
		}
		if (Boolean.TRUE.equals(isGroupByRequired)) {
			query.append("GROUP BY");
			for (String key : keys)
				query.append(" t1." + key + ",");
			if (query.charAt(query.length() - 1) == ',') {
				query = query.replace(query.length() - 1, query.length(), "");
			}
		}
		return query.toString();
	}

	@Override
	public CodeTableDatabus updateCodeTableRecord(CodeTableDatabus codeTableDatabus, MultipartFile[] files) {
		try {
			updateCodeTableChangedMap(codeTableDatabus, false);
			HashMap<String, Object> changedMap = codeTableDao.getChangedValue(files, codeTableDatabus);
			int rowsUpdated = 0;
			if (!changedMap.isEmpty()) {
				HashMap<String, Object> primaryKeyMap = codeTableDao.getPrimaryKeyValue(codeTableDatabus);
				ArrayList<Parameter> inputParam = codeTableDao.setUpdateParamValues(files, changedMap, primaryKeyMap, codeTableDatabus);
				rowsUpdated = dbEngine.executeUpdateSQL(inputParam, QueryBuilder.updateQuery(changedMap, primaryKeyMap, codeTableDatabus));
			} else {
				rowsUpdated = 1;
			}
			codeTableDatabus.setPromptMessage(rowsUpdated == 1 ? "Successfully updated code table" : null);
			codeTableDatabus.setPromptCode(1);
		} catch (Exception e) {
			codeTableDatabus.setPromptCode(0);
			codeTableDatabus.setPromptMessage(e.getMessage());
			logger.error("Exception in updateCodeTableRecord {} ", e.getMessage());
		}
		return codeTableDatabus;
	}

	@Override
	public CodeTableDatabus deleteCodeTableRecord(CodeTableDatabus codeTableDatabus) {
		if (codeTableDatabus != null) {
			updateCodeTableChangedMap(codeTableDatabus, false);
			try {
				int rowsUpdated = dbEngine.executeUpdateSQL(new ArrayList<>(), QueryBuilder.deleteQuery(codeTableDao.getChangedValue(null, codeTableDatabus), codeTableDao.getPrimaryKeyValue(codeTableDatabus), codeTableDatabus));
				codeTableDatabus.setPromptMessage(rowsUpdated == 1 ? "Successfully deleted the row" : ERROR_FOUND);
				codeTableDatabus.setPromptCode(1);
			} catch (Exception e) {
				codeTableDatabus.setPromptCode(0);
				codeTableDatabus.setPromptMessage(e.getMessage());
				logger.error("Exception in deleteCodeTableRecord {} ", e.getMessage());
			}
		}
		return codeTableDatabus;
	}

	@Override
	public CodeTableDatabus addCodeTableRecord(MultipartFile[] files, CodeTableDatabus codeTableDatabus) {
		try {
			updateCodeTableChangedMap(codeTableDatabus, true);
			HashMap<String, Object> changedMap = codeTableDao.getChangedValue(files, codeTableDatabus);
			changedMap = codeTableDao.generateMandatoryFields(codeTableDatabus, changedMap, files);
			ArrayList<Parameter> inputParam = codeTableDao.setInsertParamValues(files, changedMap, codeTableDatabus);
			String query = QueryBuilder.insertQuery(changedMap, codeTableDatabus);
			int rowsUpdated = dbEngine.executeUpdateSQL(inputParam, query);
			codeTableDatabus.getTableData().clear();
			codeTableDatabus.getTableData().add(changedMap);
			codeTableDatabus.setPromptMessage(rowsUpdated == 1 ? "Successfully added a row" : ERROR_FOUND);
			codeTableDatabus.setPromptCode(1);
		} catch (Exception e) {
			codeTableDatabus.setPromptCode(0);
			codeTableDatabus.setPromptMessage(e.getMessage());
			logger.error("Exception in addCodeTableRecord {} ", e.getMessage());
		}
		return codeTableDatabus;
	}

	@Override
	public ResponseEntity<byte[]> downloadAttachments(CodeTableDatabus codeTableDatabus, HttpServletResponse response) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			updateCodeTableChangedMap(codeTableDatabus, false);
			HashMap<String, Object> primaryKeyMap = codeTableDao.getPrimaryKeyValue(codeTableDatabus);
			String columnList = codeTableDao.getAttachmentColumn(codeTableDatabus.getCodeTable().getFields());
			String query = QueryBuilder.selectQueryForAttachment(columnList, primaryKeyMap, codeTableDatabus);
			ArrayList<HashMap<String, Object>> dataList = dbEngine.executeQuerySQL(new ArrayList<>(), query);
			if (dataList.get(0).get(codeTableDatabus.getSelectedColumnForDownload()) != null) {
				HttpHeaders headers = new HttpHeaders();
				String contentType = dataList.get(0).get("CONTENT_TYPE") == null ? codeTableDao.getContentType(dataList)
						: dataList.get(0).get("CONTENT_TYPE").toString();
				byte[] data = getByteArray(dataList.get(0).get(codeTableDatabus.getSelectedColumnForDownload()), codeTableDatabus);
				if (data != null) {
					headers.setContentType(MediaType.parseMediaType(contentType));
					String filename = dataList.get(0).get("FILE_NAME").toString();
					headers.setContentDispositionFormData(filename, filename);
					headers.setContentLength(data.length);
					headers.setCacheControl("must-revalidate, post-check=0, pre-check=0");
					headers.setPragma("public");
					attachmentData = new ResponseEntity<>(data, headers, HttpStatus.OK);
				}
			}
		} catch (Exception e) {
			codeTableDatabus.setPromptCode(0);
			codeTableDatabus.setPromptMessage(e.getMessage());
			logger.error("Exception in downloadAttachments {} ", e.getMessage());
		}
		return attachmentData;
	}

	private byte[] getByteArray(Object byteObject, CodeTableDatabus codeTableDatabus) {
		byte[] byteData = null;
		try {
			updateCodeTableChangedMap(codeTableDatabus, true);
			String datatype = QueryBuilder.getColumnDataType(codeTableDatabus.getCodeTable(),
					codeTableDatabus.getSelectedColumnForDownload());
			if (datatype.equalsIgnoreCase("Clob")) {
				String byteString = byteObject.toString();
				byteData = byteString.getBytes();
			} else {
				if (oracledb.equalsIgnoreCase("Y")) {
					byteData = (byte[]) byteObject;
				} else {
					ByteArrayOutputStream byteArrayOutputStream = null;
					byteArrayOutputStream = (ByteArrayOutputStream) byteObject;
					byteData = byteArrayOutputStream.toByteArray();
				}
			}
		} catch (Exception e) {
			logger.error("Exception in getByteArray {} ", e.getMessage());
		}
		return byteData;
	}

	@Override
	public String updateCodeTableRecordForWaf(CodeTableDatabus codeTableDatabus) {
		try {
			MultipartFile multipartFile = null;
			String contentType = null;
			String splicedFile = codeTableDatabus.getFileContent();
			String name = codeTableDatabus.getFileName();
			Integer remaining = codeTableDatabus.getRemaining();
			Integer length = codeTableDatabus.getLength();
			String userId = codeTableDatabus.getPersonId();
			String timestamp = codeTableDatabus.getFileTimestamp();
			if (splicedFile != null) {
				contentType = codeTableDatabus.getContentType();
				multipartFile = commonService.uploadMedia(splicedFile, name, remaining, length, timestamp, userId, contentType);
			}
			if ((multipartFile != null && !multipartFile.isEmpty()) || splicedFile == null) {
				updateCodeTableChangedMap(codeTableDatabus, false);
				HashMap<String, Object> changedMap = codeTableDao.getChangedValueForWaf(multipartFile, codeTableDatabus);
				int rowsUpdated = 0;
				if (!changedMap.isEmpty()) {
					HashMap<String, Object> primaryKeyMap = codeTableDao.getPrimaryKeyValue(codeTableDatabus);
					ArrayList<Parameter> inputParam = codeTableDao.setUpdateParamValuesForWaf(multipartFile, changedMap, primaryKeyMap, codeTableDatabus);
					String query = QueryBuilder.updateQuery(changedMap, primaryKeyMap, codeTableDatabus);
					rowsUpdated = dbEngine.executeUpdateSQL(inputParam, query);
				} else {
					rowsUpdated = 1;
				}
				codeTableDatabus.setPromptMessage(rowsUpdated == 1 ? "Successfully updated code table" : null);
				codeTableDatabus.setPromptCode(1);
			}
		} catch (Exception e) {
			codeTableDatabus.setPromptCode(0);
			codeTableDatabus.setPromptMessage(e.getMessage());
			logger.error("Exception in updateCodeTableRecord {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(codeTableDatabus);
	}

	@Override
	public String addCodeTableRecordForWaf(CodeTableDatabus codeTableDatabus) {
		try {
			MultipartFile multipartFile = null;
			String contentType = null;
			String splicedFile = codeTableDatabus.getFileContent();
			String name = codeTableDatabus.getFileName();
			Integer remaining = codeTableDatabus.getRemaining();
			Integer length = codeTableDatabus.getLength();
			String userId = codeTableDatabus.getPersonId();
			String timestamp = codeTableDatabus.getFileTimestamp();
			if (splicedFile != null) {
				contentType = codeTableDatabus.getContentType();
				multipartFile = commonService.uploadMedia(splicedFile, name, remaining, length, timestamp, userId, contentType);
			}
			if ((multipartFile != null && !multipartFile.isEmpty()) || splicedFile == null) {
				updateCodeTableChangedMap(codeTableDatabus, true);
				HashMap<String, Object> changedMap = codeTableDao.getChangedValueForWaf(multipartFile, codeTableDatabus);
				changedMap = codeTableDao.generateMandatoryFieldsForWaf(codeTableDatabus, changedMap, multipartFile);
				ArrayList<Parameter> inputParam = codeTableDao.setInsertParamValuesForWaf(multipartFile, changedMap, codeTableDatabus);
				int rowsUpdated = dbEngine.executeUpdateSQL(inputParam, QueryBuilder.insertQuery(changedMap, codeTableDatabus));
				codeTableDatabus.getTableData().clear();
				codeTableDatabus.getTableData().add(changedMap);
				codeTableDatabus.setPromptMessage(rowsUpdated == 1 ? "Successfully added a row" : ERROR_FOUND);
				codeTableDatabus.setPromptCode(1);
			}
		} catch (Exception e) {
			codeTableDatabus.setPromptCode(0);
			codeTableDatabus.setPromptMessage(e.getMessage());
			logger.error("Exception in addCodeTableRecord {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(codeTableDatabus);
	}

	@Override
	public CodeTableDatabus getCodeTableConfiguration() {
		CodeTableDatabus codeTableDatabus = new CodeTableDatabus();
		codeTableDatabus.setCodeTableConfigurations(codeTableDao.loadCodeTableConfiguration());
		return codeTableDatabus;
	}

	private CodeTable convertStringToCodeTableObject(String content) {
		CodeTable codeTable = new CodeTable();
		ObjectMapper mapper = new ObjectMapper();
		try {
			codeTable = mapper.readValue(content, CodeTable.class);
		} catch (Exception e) {
			logger.error("error occured in getDataFromReprotJsonByTypeId : {}", e.getMessage());
		}
		return codeTable;
	}

	private void fetchCodeTableObject(CodeTableDatabus codeTableDatabus, Boolean isRemovalRequired) {
		CodeTable codeTable = convertStringToCodeTableObject(codeTableDao.fetchCodeTableConfigurationByTableName(codeTableDatabus.getTableName()).getContent());
		if (Boolean.TRUE.equals(isRemovalRequired)) {
			removeUnWantedDataFromCodeTable(codeTableDatabus, codeTable);
		} else {
			codeTableDatabus.setCodeTable(codeTable);
		}
	}

	private void removeUnWantedDataFromCodeTable(CodeTableDatabus codeTableDatabus, CodeTable codeTable) {
		List<Fields> fields = new ArrayList<>(codeTable.getFields());
		List<String> primaryKey = new ArrayList<>(codeTable.getPrimaryKey());
		CodeTable updatedCodeTable = new CodeTable();
		BeanUtils.copyProperties(codeTable, updatedCodeTable);
		updatedCodeTable.getFields().clear();
		fields.forEach(fieldData -> {
			if (primaryKey.contains(fieldData.getColumnName()) || Boolean.TRUE.equals(fieldData.getIsEditable())) {
				updatedCodeTable.getFields().add(fieldData);
			}
		});
		codeTableDatabus.setCodeTable(updatedCodeTable);
	}

	private void updateCodeTableChangedMap(CodeTableDatabus codeTableDatabus, Boolean isRemovalRequired) {
		fetchCodeTableObject(codeTableDatabus, isRemovalRequired);
		List<String> changedMap = codeTableDatabus.getChangedMap();
		List<Fields> fields = codeTableDatabus.getCodeTable().getFields();
		fields.forEach(fieldData -> {
			if (changedMap.contains(fieldData.getColumnName())) {
				fieldData.setValueChanged(true);
			}
		});
	}
}
