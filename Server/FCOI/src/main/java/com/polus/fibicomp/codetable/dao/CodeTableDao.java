package com.polus.fibicomp.codetable.dao;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.codetable.dto.CodeTableDatabus;
import com.polus.fibicomp.codetable.dto.Fields;
import com.polus.fibicomp.codetable.pojo.CodeTableConfiguration;
import com.polus.fibicomp.dbengine.Parameter;

public interface CodeTableDao {
	
	 /**
	 * @param fields
	 * @param primaryKeys
	 * @return Get visible columns for table
	 */
	String getColumnList(List<Fields> fields, List<String> primaryKeys);
	
	/**
	 * @param codeTableDatabus
	 * @return Get changed values
	 */
	HashMap<String, Object> getChangedValue(MultipartFile[] files,CodeTableDatabus codeTableDatabus);

	/**
	 * @param codeTableDatabus
	 * @return Get primary key and its value
	 */
	HashMap<String, Object> getPrimaryKeyValue(CodeTableDatabus codeTableDatabus);

	/**
	 * @param primaryKeyList
	 * @param updatedMap 
	 * @param fieldList 
	 * @param changedMap 
	 * @param inParam
	 * @return set input variables
	 */
	ArrayList<Parameter> getInputVariable(List<String> primaryKeyList, HashMap<String, Object> updatedMap, List<Fields> fieldList, HashMap<String, String> changedMap, ArrayList<Parameter> inParam);

	/**
	 * @param codeTableDatabus
	 * @param changedMap
	 * @param files 
	 * @return generate Primary key of table
	 */
	HashMap<String, Object> generateMandatoryFields(CodeTableDatabus codeTableDatabus, HashMap<String, Object> changedMap, MultipartFile[] files);

	/**
	 * @param fields
	 * @return get attachment column
	 */
	String getAttachmentColumn(List<Fields> fields);

	/**
	 * @param dataList
	 * @return get content type of a attachment
	 */
	String getContentType(ArrayList<HashMap<String, Object>> dataList);

	/**
	 * @param files
	 * @param changedMap
	 * @param codeTableDatabus
	 * @return
	 */
	ArrayList<Parameter> setInsertParamValues(MultipartFile[] files, HashMap<String, Object> changedMap,
			CodeTableDatabus codeTableDatabus);

	/**
	 * @param files
	 * @param changedMap
	 * @param primaryKeyMap
	 * @param codeTableDatabus
	 * @return
	 */
	ArrayList<Parameter> setUpdateParamValues(MultipartFile[] files, HashMap<String, Object> changedMap,
			HashMap<String, Object> primaryKeyMap, CodeTableDatabus codeTableDatabus);

	/**
	 * @param codeTableDatabus
	 * @return Get changed values
	 */
	HashMap<String, Object> getChangedValueForWaf(MultipartFile files,CodeTableDatabus codeTableDatabus);

	/**
	 * @param files
	 * @param changedMap
	 * @param primaryKeyMap
	 * @param codeTableDatabus
	 * @return
	 */
	ArrayList<Parameter> setUpdateParamValuesForWaf(MultipartFile files, HashMap<String, Object> changedMap, HashMap<String, Object> primaryKeyMap, CodeTableDatabus codeTableDatabus);

	/**
	 * @param files
	 * @param changedMap
	 * @param codeTableDatabus
	 * @return
	 */
	ArrayList<Parameter> setInsertParamValuesForWaf(MultipartFile files, HashMap<String, Object> changedMap, CodeTableDatabus codeTableDatabus);

	/**
	 * @param codeTableDatabus
	 * @param changedMap
	 * @param files 
	 * @return generate Primary key of table
	 */
	HashMap<String, Object> generateMandatoryFieldsForWaf(CodeTableDatabus codeTableDatabus, HashMap<String, Object> changedMap, MultipartFile files);

	/**
	 * @return
	 */
	public List<CodeTableConfiguration> loadCodeTableConfiguration();

	/**
	 * @param tableName
	 * @return
	 */
	public CodeTableConfiguration fetchCodeTableConfigurationByTableName(String tableName);

}
