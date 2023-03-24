package com.polus.fibicomp.report.dao;

import java.util.HashMap;
import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.report.pojo.ReportColumns;
import com.polus.fibicomp.report.pojo.ReportTemplate;
import com.polus.fibicomp.vo.CommonVO;

@Transactional
@Service
public interface ReportDao {

	/**
	 * This method is used to save or update the user defined report template
	 * @param reportTemplate
	 * @return saved report template
	 */
	public ReportTemplate saveOrUpdateReportTemplate(ReportTemplate reportTemplate);
	
	/**
	 * This method is used to get the report by report template id and person id
	 * @param reportTemplateId
	 * @param personId
	 * @return object of ReportTemplate
	 */
	public ReportTemplate getReportTemplateByIdAndPersonId(Integer reportTemplateId, String personId);


	/**
	 * This method is used to fetch the report template created by the user and system defined templates
	 * @param personId
	 * @return list of report templates
	 */
	public List<ReportTemplate> fetchAllReportTemplatesBasedOnPersonId(String personId);

	/**
	 * This method is used to generate the report based on the given query
	 * @param query
	 * @return list of data from the report view
	 */
	public List<HashMap<String, Object>> generateReportByQuery(String query);

	/**
	 * This method is used to get the data for export report as list of Object
	 * @param query
	 * @return list of object
	 */
	public List<Object[]> downloadReportByQuery(String query);

	/**
	 * This method is used to fetch the report template based on report template id
	 * @param reportTemplateId
	 * @return object of ReportTemplate
	 */
	public ReportTemplate getReportTemplateById(Integer reportTemplateId);

	/**
	 * This method is used to get the report template JSON name based on report type
	 * @param reportTypeId
	 * @return name of JSON file
	 */
	public String getReportTemplateJsonByReportType(String reportTypeId);

	public String getJoinClause(String reportTypeCode);

	/**
	 * This method is used to delete the report template
	 * @param reportTemplate
	 * @return message
	 */
	public String deleteReportTemplate(ReportTemplate reportTemplate);

	/**
	 * This method is used to fetch All Report Templates
	 * @return list of report templates
	 */
	public List<ReportTemplate> fetchAllReportTemplates();

	/**
	 * This method is used to get administrative details  from diff role based tables
	 * @param vo
	 * @return Object[]
	 */
	List<Object[]> administrativeDetails(CommonVO vo);

	/**
	 * This method is used to get the input parameter details based on the report type id
	 * @param reportTypeId
	 * @return report input parameter details for BIRT
	 */
	public List<ReportColumns> getReportColumnsByType(String reportTypeId);
}
