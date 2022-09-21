
package com.polus.fibicomp.report.service;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.security.spec.KeySpec;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.concurrent.atomic.AtomicInteger;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.openxml4j.opc.PackagePart;
import org.apache.poi.openxml4j.opc.PackagePartName;
import org.apache.poi.openxml4j.opc.PackagingURIHelper;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CreationHelper;
import org.apache.poi.ss.usermodel.DataFormat;
import org.apache.poi.ss.usermodel.Footer;
import org.apache.poi.ss.usermodel.Header;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.RegionUtil;
import org.apache.poi.xssf.streaming.SXSSFCell;
import org.apache.poi.xssf.streaming.SXSSFRow;
import org.apache.poi.xssf.streaming.SXSSFSheet;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFFont;
import org.apache.poi.xssf.usermodel.XSSFPictureData;
import org.apache.poi.xssf.usermodel.XSSFRelation;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.json.JSONException;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.ResourceAccessException;
import org.springframework.web.client.RestTemplate;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.claims.claimsIntegration.excelity.service.ExcelityService;
import com.polus.fibicomp.codetable.dao.JSONParser;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dashboard.service.DashboardService;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.report.dao.ReportDao;
import com.polus.fibicomp.report.pojo.AuditReportType;
import com.polus.fibicomp.report.pojo.ReportColumns;
import com.polus.fibicomp.report.pojo.ReportTemplate;
import com.polus.fibicomp.report.repository.AuditReportTypeRepository;
import com.polus.fibicomp.report.vo.Field;
import com.polus.fibicomp.report.vo.ReportJSON;
import com.polus.fibicomp.report.vo.ReportVO;
import com.polus.fibicomp.report.vo.ReqObject;
import com.polus.fibicomp.report.vo.VmlDrawing;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.CommonVO;

@Transactional
@Service(value = "reportService")
public class ReportServiceImpl implements ReportService {

	protected static Logger logger = LogManager.getLogger(ReportServiceImpl.class.getName());

	@Autowired
	@Qualifier(value = "reportDao")
	private ReportDao reportDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private JSONParser jsonParser;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private ExcelityService excelityService;

	@Value("${manpower.excelity.saltValue}")
	private String saltValue;

	@Value("${birt.report.url}")
	private String birtUrl;

	@Autowired
	private CommonService commonService;

	@Autowired
	private DashboardService dashboardService;

	@Autowired
	private AuditReportTypeRepository auditReportTypeRepository;

	@Autowired
	private EmailService emailService;

	@Override
	public ReportVO fetchReportDetails(ReportVO vo) {
		try {
			vo.setConfigFile(jsonParser.getJSONDataForReport(reportDao.getReportTemplateJsonByReportType(vo.getReportTypeId())));
		} catch (Exception e) {
			vo.setPromptCode(0);
			vo.setPromptMessage(e.getMessage());
			logger.error("Exception in fetchReportDetails : {}", e.getMessage());
		}
		return vo;
	}

	@Override
	public String saveOrUpdateReportTemplate(ReportVO vo) {
		ReportTemplate reportTemplate = vo.getReportTemplate();
		if (reportTemplate.getReportTemplateId() == null) {
			reportTemplate.setCreateTimestamp(commonDao.getCurrentTimestamp());
			reportTemplate.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		} else {
			reportTemplate.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		}
		reportTemplate.setUpdateUserFullName(personDao.getUserFullNameByUserName(reportTemplate.getUpdateUser()));
		reportDao.saveOrUpdateReportTemplate(reportTemplate);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getReportTemplateById(ReportVO vo, String token) {
		ReportTemplate reportTemplate = null;
		if (Boolean.TRUE.equals(vo.getIsReportAdmin())) {
			reportTemplate = reportDao.getReportTemplateById(vo.getReportTemplateId());
		} else {
			reportTemplate = reportDao.getReportTemplateByIdAndPersonId(vo.getReportTemplateId(), vo.getPersonId());
		}
		if (reportTemplate != null) {
			reportTemplate.setUpdateUserFullName(personDao.getUserFullNameByUserName(reportTemplate.getUpdateUser()));
		}
		vo.setReportTemplate(reportTemplate);
		if (reportTemplate != null && vo.getIsBirt() != null && Boolean.TRUE.equals(vo.getIsBirt())) {
			vo.setInputParamDetails(getParameterDetails(reportTemplate.getTypeCode(), token));
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String fetchAllReportTemplates(ReportVO vo) {
		if (Boolean.TRUE.equals(vo.getIsReportAdmin())) {
			vo.setReportTemplates(reportDao.fetchAllReportTemplates());
		} else {
			vo.setReportTemplates(reportDao.fetchAllReportTemplatesBasedOnPersonId(vo.getPersonId()));
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String generateReport(ReportVO vo) {
		Map<String, String> decryptedData  = new HashMap<>();
		String query = generateQuery(vo.getSelectedFields(), vo.getReportView(), vo.getConditions(), vo.getPersonId(), vo.getReportTypeId(), vo.getJoinWhereClause(), true);
		if (query == null) {
			ReportTemplate reportTemplate = reportDao.getReportTemplateById(vo.getReportTemplateId());
			query = reportTemplate.getTemplateSql();
		}
		if (vo.getSpecialIndex() != null && !vo.getSpecialIndex().isEmpty()) {
			try {
				String key = getDecryptedSecretKeyAES();
				ArrayList<HashMap<String, Object>> dataList = (ArrayList<HashMap<String, Object>>) reportDao.generateReportByQuery(query);
				List<String> keys = Arrays.asList(vo.getSelectedFields().trim().split("\\s*,\\s*"));
				dataList.stream().forEach(data -> decryptSpecialFields(data, keys, vo.getSpecialIndex(), decryptedData, key));
				vo.setGeneratedReport(dataList);
			} catch (Exception e) {
				logger.error("error occued while decryption");
			}
		} else {
			vo.setGeneratedReport(reportDao.generateReportByQuery(query));
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private void decryptSpecialFields(HashMap<String, Object> data, List<String> keys, List<Integer> specialIndexes, Map<String, String> decryptedData, String decryptKey) {
		specialIndexes.stream().forEach(specialIndex -> {
			try {
				if (data.get(keys.get(specialIndex)) != null) {
					if (decryptedData.get(data.get(keys.get(specialIndex))) != null) {
						data.put(keys.get(specialIndex), decryptedData.get(data.get(keys.get(specialIndex))));
					} else {
						decryptedData.put(data.get(keys.get(specialIndex)).toString(), decryptAESData((String) data.get(keys.get(specialIndex)), decryptKey));
						data.put(keys.get(specialIndex), decryptedData.get(data.get(keys.get(specialIndex))));
					}
				}
			} catch (Exception e) {
				logger.error("error occued while decryption");
			}
		});
	}

	private String generateQuery(String selectedFields, String reportView, String conditions, String personId, String reportTypeCode, String joinWhereClause, boolean needLimit) {
		if (selectedFields == null && reportView == null && conditions == null) {
			return null;
		}
		ReportJSON reportJson = getDataFromReprotJsonByTypeId(reportTypeCode);
		String joinClause = reportDao.getJoinClause(reportTypeCode);
		String selectDistinct = reportJson.getSelectDistinct();
		String query = String.join(" ", "SELECT",
				(selectDistinct != null && selectDistinct.equals("Y") ? "DISTINCT" : ""),
				(selectedFields == null ? "*" : selectedFields), reportJson.getExtraSelection(), "FROM", reportView,
				joinClause, conditions == null || conditions.equals("") ? "WHERE" : conditions + " AND",
				reportJson.getExtraCondition(), (needLimit ? "LIMIT 200" : "" ));
		if (joinWhereClause != null && reportJson.getInnerJoinPlaceHolder() != null) {
			query = query.replaceAll(reportJson.getInnerJoinPlaceHolder(), joinWhereClause);
		}
		return query.replace("<<PERSON_ID>>", "'"+ personId + "'");
	}

	@Override
	public ResponseEntity<byte[]> exportGeneratedReport(ReportVO vo) {
		try {
			String query = generateQuery(vo.getSelectedFields(), vo.getReportView(), vo.getConditions(), vo.getPersonId(), vo.getReportTypeId(), vo.getJoinWhereClause(), false);
			List<String> tableHeadingRow = vo.getHeaders();
			if (query == null) {
				ReportTemplate reportTemplate = reportDao.getReportTemplateById(vo.getReportTemplateId());
				query = reportTemplate.getTemplateSql();
				tableHeadingRow = getReportColumnHeadings(reportTemplate.getTemplateJson());
			}
			SXSSFWorkbook workbook = new SXSSFWorkbook();
			SXSSFSheet sheet = workbook.createSheet("Report");
			String downloadQuery = queryWithSelectedFields(vo.getSelectedFields(), query);
			List<Object[]> reportDatas = reportDao.downloadReportByQuery(downloadQuery);
			List<Object[]> rowOfTotal = caluculateTotalOfSummableFieldForExport(vo, downloadQuery);
			Boolean showTotalRow = (reportDatas.isEmpty() || rowOfTotal.isEmpty()) ? Boolean.FALSE : Boolean.TRUE;
			if (!reportDatas.isEmpty()) {
				reportDatas.addAll(rowOfTotal);
			}
			getHeaderDetailsToExport(vo);
			prepareExcelSheet(reportDatas, sheet, tableHeadingRow.toArray(), workbook, vo, showTotalRow);
			CommonVO commonVO = new CommonVO();
			commonVO.setExportType("xlsx");
			return getResponseEntityForDownload(commonVO, workbook);
		} catch (Exception e) {
			logger.error("Error occured in exportGeneratedReport : {}", e.getMessage());
			return null;
		}
	}

	private String queryWithSelectedFields(String selectedFields, String query) {
		String columnNames = removeTheAliasNameFromColumnName(selectedFields);
		return "SELECT " + columnNames + " FROM (" + query + ") TAB";
	}

	private String removeTheAliasNameFromColumnName(String selectedFields) {
		if (!selectedFields.contains(".")) {
			return selectedFields;
		}
		StringBuilder sb = new StringBuilder();
		List<String> columnNamesWithAlias = Arrays.asList(selectedFields.split("\\s*,\\s*"));
		for (String columnNameWithAlias :columnNamesWithAlias) {
			sb.append(StringUtils.substringAfter(columnNameWithAlias, "."));
			sb.append(",");
		}
		return sb.toString().replaceAll(",$", "");
	}

	@SuppressWarnings("unchecked")
	private List<String> getReportColumnHeadings(String templateJson) {
		Map<String, Object> retMap = new HashMap<>();
		List<String> columnHeadings = new ArrayList<>();
		try {
			JSONObject jsonObject = new JSONObject(templateJson);
			if (jsonObject != JSONObject.NULL) {
				retMap = JSONParser.toMap(jsonObject);
			}
			columnHeadings = (List<String>) retMap.get("headers");
		} catch (JSONException e) {
			logger.info("exception in getReportColumnHeadings : {}", e.getMessage());
		}
		return columnHeadings;
	}

	private List<Object[]> caluculateTotalOfSummableFieldForExport(ReportVO vo, String query) {
		String selectedFields = generateSummableQuery(vo.getSelectedFields(), vo.getReportTypeId());
		query= queryWithSelectedFields(selectedFields, query);
		if (query!= null && query.contains("SUM(")) {
			return reportDao.downloadReportByQuery(queryWithSelectedFields(vo.getSelectedFields(), query));
		} else {
			return new ArrayList<>();
		}
	}

	private String generateSummableQuery(String selectedFields, String reportTypeId) {
		StringBuilder summableQuery = new StringBuilder("");
		try {
			List<String> selectedFieldsForTotal = new ArrayList<>(Arrays.asList(selectedFields.split(", ")));
			ReportJSON reportJson = getDataFromReprotJsonByTypeId(reportTypeId);
			for (Field field : reportJson.getReports()) {
				for (String selectedFieldForTotal : selectedFieldsForTotal) {
					String columnName = selectedFieldForTotal;
					if (selectedFieldForTotal.contains(".")) {
						columnName = StringUtils.substringAfter(selectedFieldForTotal, ".");
					}
					if (selectedFieldForTotal.trim().equals(field.getColumnName()) && field.getSummable() != null && field.getSummable().equals("Y")) {
						summableQuery.append("SUM(" + columnName + ") AS " + columnName + " ,");
					} else if (selectedFieldForTotal.trim().equals(field.getColumnName())
							&& field.getSummable() != null  && field.getSummable().equals("N")) {
						summableQuery.append("NULL AS " + columnName + " ,");
					}
				}
			}
			return summableQuery.toString().replaceAll(",$", "");
		} catch (Exception e) {
			logger.info("error occured in generateSummableQuery : {}", e.getMessage());
		}
		return null;
	}

	private ReportJSON getDataFromReprotJsonByTypeId(String reportTypeId) {
		ObjectMapper mapper = new ObjectMapper();
		ReportJSON reportJson = new ReportJSON();
		try {
			reportJson = mapper.readValue(jsonParser.getJSONDataByFileName(reportDao.getReportTemplateJsonByReportType(reportTypeId)).toString(), ReportJSON.class);
		} catch (Exception e) {
			logger.error("error occured in getDataFromReprotJsonByTypeId : {}", e.getMessage());
		}
		return reportJson;
	}

	public ReportVO getHeaderDetailsToExport(ReportVO vo) {
		ReportTemplate reportTemplate = reportDao.getReportTemplateById(vo.getReportTemplateId());
		Map<String, String> headerDetails = new HashMap<>();
		headerDetails.put("Report Title", reportTemplate.getTemplateName());
		Date date = new Date();
        DateFormat df = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss (z)");
        df.setTimeZone(TimeZone.getTimeZone(Constants.CRON_JOB_TIMEZONE));
		headerDetails.put("Generated By", personDao.getPersonFullNameByPersonId(vo.getPersonId()) + " on "+ df.format(date));
		vo.setHeaderDetails(headerDetails);
		vo.setDocumentHeading(vo.getDocumentHeading());
		return vo;
	}

	public SXSSFWorkbook prepareExcelSheet(List<Object[]> reportDatas, SXSSFSheet sheet, Object[] tableHeadingRow, SXSSFWorkbook workbook, ReportVO vo, Boolean showTotalRow) {
		XSSFCellStyle tableBodyStyle = (XSSFCellStyle) workbook.createCellStyle();
		Map<String, String> decryptedData  = new HashMap<>();
		String documentHeading = vo.getDocumentHeading();
		addDetailsInHeader(workbook, sheet, vo.getHeaderDetails());
		logger.info("documentHeading : {}", documentHeading);
		int startingCell = 0;
		CellStyle dateStyle = workbook.createCellStyle();
		XSSFCellStyle currencyStyle = (XSSFCellStyle) workbook.createCellStyle();
		setCriteiaDetails(workbook, vo.getReportCriteria(), vo.getHeaderDetails());
		int rowNumber = prepareExcelSheetHeader(sheet, tableHeadingRow, workbook, tableBodyStyle, 0);
		AtomicInteger rowIndex= new AtomicInteger(rowNumber);
		if(vo.getSpecialIndex() != null && !vo.getSpecialIndex().isEmpty() && !vo.getSelectedFields().isEmpty()) {
			try {
				String key = getDecryptedSecretKeyAES();
				reportDatas.stream().forEach(reportData-> {
					addDataToSheet(reportData, rowIndex, sheet, workbook,currencyStyle, startingCell, tableBodyStyle,dateStyle, vo.getSpecialIndex(), decryptedData, key);
					rowIndex.incrementAndGet();
				});
			} catch (Exception e) {
				logger.error("error occued while decryption");
			}
		} else {
			reportDatas.stream().forEach(reportData-> {
				addDataToSheet(reportData, rowIndex, sheet, workbook,currencyStyle, startingCell, tableBodyStyle,dateStyle, null, decryptedData, null);
				rowIndex.incrementAndGet();
				});
		}
		setBorderBeforeTotalRow(rowIndex.decrementAndGet(), reportDatas, showTotalRow, sheet);
		return workbook;
	}

	private void addDataToSheet(Object[] objectDatas, AtomicInteger rowNumber, SXSSFSheet sheet, SXSSFWorkbook workbook, XSSFCellStyle currencyStyle,
			int startingCell, XSSFCellStyle tableBodyStyle, CellStyle dateStyle, List<Integer> specialIndex, Map<String, String> decryptedData, String decryptedKey) {
		SXSSFRow row = sheet.createRow(rowNumber.get());
		int cellNumber = startingCell;
		for (Object objectData : objectDatas) {
			SXSSFCell cell = row.createCell(cellNumber);
			cell.setCellStyle(tableBodyStyle);
			if (objectData instanceof String) {
				if(specialIndex != null && !specialIndex.isEmpty() && specialIndex.contains(cellNumber)) {
					try {
						if (decryptedData.get(objectData.toString()) != null) {
							cell.setCellValue(decryptedData.get(objectData.toString()));
						} else {
							decryptedData.put(objectData.toString(), decryptAESData((String)objectData, decryptedKey));
							cell.setCellValue(decryptedData.get(objectData.toString()));
						}
					} catch (Exception e) {
						logger.info("error occured while decrypt the data");
					}
				} else {
					cell.setCellValue((String) objectData);
				}
			}
			else if (objectData instanceof Integer) {
				cell.setCellValue((Integer) objectData);
			}
			else if (objectData instanceof BigInteger) {
				String stringValue = ((BigInteger) objectData).toString();
				cell.setCellValue(stringValue);
			}
			else if (objectData instanceof BigDecimal) {
				currencyStyle.setAlignment(HorizontalAlignment.RIGHT);
				DataFormat df = workbook.createDataFormat();
				currencyStyle.setDataFormat(df.getFormat("#,#0.00"));
				cell.setCellStyle(currencyStyle);
				cell.setCellValue(((BigDecimal) objectData).doubleValue());
			}  else if (objectData instanceof java.sql.Date) {
				if (objectData != null) {
					Date date = (Date) objectData;
					cell.setCellValue(date);
					CreationHelper creationHelper = workbook.getCreationHelper();
					dateStyle.setDataFormat(creationHelper.createDataFormat().getFormat("dd-mm-yyyy"));
					cell.setCellStyle(dateStyle);
				}
			}  else if (objectData instanceof Date) {
				if (objectData != null) {
					Date date = (Date) objectData;
					String format = "dd-MM-yyyy HH:mm:ss";
				    SimpleDateFormat estFormatter = new SimpleDateFormat(format);
				    estFormatter.setTimeZone(TimeZone.getTimeZone(Constants.CRON_JOB_TIMEZONE));
				    try {
						Date dateWithTimeZone = estFormatter.parse(String.valueOf(estFormatter.format(date)));
						SimpleDateFormat newFormat = new SimpleDateFormat(format);
						dateWithTimeZone = newFormat.parse(String.valueOf(estFormatter.format(dateWithTimeZone)));
						cell.setCellValue(dateWithTimeZone);
					} catch (ParseException e) {
						logger.error("error occured while converting the date in prepareExcelSheet : {}", e.getMessage() );
					}
					CreationHelper creationHelper = workbook.getCreationHelper();
					dateStyle.setDataFormat(creationHelper.createDataFormat().getFormat("dd-mm-yyyy"));
					cell.setCellStyle(dateStyle);
				}
			} else if (objectData == null) {
				cell.setCellValue(" ");
			} else if (objectData instanceof Double) {
				currencyStyle.setAlignment(HorizontalAlignment.RIGHT);
				DataFormat df = workbook.createDataFormat();
				currencyStyle.setDataFormat(df.getFormat("#,#0.00"));
				cell.setCellStyle(currencyStyle);
				cell.setCellValue(((Double) objectData).doubleValue());
			} 
			cellNumber++;
		}
	}

	private int setCriteiaDetails(SXSSFWorkbook workbook, List<String> reportCriterias, Map<String, String> criteriaDetails) {
		int rowNumber = 0;
		int cellNumber = 0;
		if (reportCriterias != null && !reportCriterias.isEmpty()) {
			SXSSFSheet criteriaSheet = workbook.createSheet("Criteria");
			XSSFCellStyle criteriaHeadStyle = (XSSFCellStyle) workbook.createCellStyle();
			XSSFFont tableHeadFont = (XSSFFont) workbook.createFont();
			tableHeadFont.setBold(true);
			tableHeadFont.setFontHeightInPoints((short) 12);
			criteriaHeadStyle.setFont(tableHeadFont);
			SXSSFRow criteriaRow = criteriaSheet.createRow(rowNumber);
			SXSSFCell cell = criteriaRow.createCell(cellNumber);
			cell.setCellValue("Criteria Used In Report");
			cell.setCellStyle(criteriaHeadStyle);
			rowNumber++;
			StringBuilder criteria = new StringBuilder();
			for (String reportCriteria : reportCriterias) {
				criteria.append(reportCriteria);
				criteria.append(" \n");
			}
			XSSFCellStyle criteriaStyle = (XSSFCellStyle) workbook.createCellStyle();
			criteriaRow = criteriaSheet.createRow(rowNumber);
			cell = criteriaRow.createCell(cellNumber);
			cell.setCellValue(criteria.toString());
			criteriaStyle.setWrapText(true);
			cell.setCellStyle(criteriaStyle);
			criteriaSheet.addMergedRegion(new CellRangeAddress(1, reportCriterias.size(), 0, 20));
			addDetailsInHeader(workbook, criteriaSheet, criteriaDetails);
		}
		return rowNumber;
	}

	private void setBorderBeforeTotalRow(int rowNumber, List<Object[]> dashboardData, Boolean showTotalRow, SXSSFSheet sheet) {
		if (showTotalRow.equals(Boolean.TRUE)) {
			CellRangeAddress  range = new CellRangeAddress(rowNumber,rowNumber,0,(dashboardData.get(0)).length-1);
	        RegionUtil.setBorderTop(BorderStyle.MEDIUM, range, sheet);
		}
	}

	private int prepareExcelSheetHeader(SXSSFSheet sheet, Object[] tableHeadingRow, SXSSFWorkbook workbook, XSSFCellStyle tableBodyStyle,  int rowNumber) {
		int headingCellNumber = 0;
		int rowIndex = rowNumber;
		// Table head style and font creation code.
		SXSSFRow tableHeadRow = sheet.createRow(rowIndex);
		rowIndex ++;
		XSSFCellStyle tableHeadStyle = (XSSFCellStyle) workbook.createCellStyle();
		tableHeadStyle.setBorderTop(BorderStyle.HAIR);
		tableHeadStyle.setBorderBottom(BorderStyle.HAIR);
		tableHeadStyle.setBorderLeft(BorderStyle.HAIR);
		tableHeadStyle.setBorderRight(BorderStyle.HAIR);
		XSSFFont tableHeadFont = (XSSFFont) workbook.createFont();
		tableHeadFont.setBold(true);
		tableHeadFont.setFontHeightInPoints((short) 12);
		tableHeadStyle.setFont(tableHeadFont);
		// set by arjun REPORT
		tableHeadStyle.setWrapText(true);
		// Table body style and font creation code.
		tableBodyStyle.setBorderTop(BorderStyle.HAIR);
		tableBodyStyle.setBorderBottom(BorderStyle.HAIR);
		tableBodyStyle.setBorderLeft(BorderStyle.HAIR);
		tableBodyStyle.setBorderRight(BorderStyle.HAIR);
		XSSFFont tableBodyFont = (XSSFFont) workbook.createFont();
		tableBodyFont.setFontHeightInPoints((short) 12);
		tableBodyStyle.setFont(tableBodyFont);
		// Set table head data to each column.
		for (Object heading : tableHeadingRow) {
			SXSSFCell cell = tableHeadRow.createCell(headingCellNumber);
			cell.setCellValue((String) heading);
			cell.setCellStyle(tableHeadStyle);
			// set by arjun REPORT
			sheet.setColumnWidth(headingCellNumber, 2600);
			headingCellNumber++;
		}
		return rowIndex;
	}

	public void addDetailsInHeader(SXSSFWorkbook workbook, SXSSFSheet sheet, Map<String, String> headerDetails) {
		try {
			  Header header;
			  InputStream is;
			  byte[] bytes;
			  int pictureIdx;
			  header = sheet.getHeader();
			  header.setLeft("&G");
			  header.setCenter( "&K000000&12"+Constants.REPORT_HEADER +"\n &10 " +headerDetails.get("Report Title")+"\n &8Generated By : " +headerDetails.get("Generated By"));
			  Footer footer;
			  footer = sheet.getFooter();
			  footer.setCenter(Constants.REPORT_FOOTER);
			  Resource resource = new ClassPathResource("logo.png");
			  is = resource.getInputStream();
			  bytes = IOUtils.toByteArray(is);
			  pictureIdx = workbook.addPicture(bytes, Workbook.PICTURE_TYPE_PNG);
			  is.close();
			  sheet.setMargin(org.apache.poi.ss.usermodel.Sheet.TopMargin, 1);
			  //create header picture from picture data of this workbook
			  createPictureForHeader(sheet, pictureIdx, "logo", 1, "LH", sheet.getSheetName());
			} catch (Exception e) {
				logger.info("Error Occured in createPictureForHeader : {}", e.getMessage());
			}
		}


	private void createPictureForHeader(SXSSFSheet sheet, int pictureIdx, String pictureTitle, int vmlIdx, String headerPos, String sheetName) {
		try {
			OPCPackage opcpackage = sheet.getWorkbook().getXSSFWorkbook().getPackage();

			// creating /xl/drawings/vmlDrawing1.vml
			String partSheetName = new StringBuilder("/xl/drawings/vmlDrawing").append(vmlIdx).append(sheetName).append(".vml").toString();
			PackagePartName partname = PackagingURIHelper.createPartName(partSheetName);
			PackagePart part = opcpackage.createPart(partname, "application/vnd.openxmlformats-officedocument.vmlDrawing");
			// creating new VmlDrawing
			VmlDrawing vmldrawing = new VmlDrawing(part);
			// creating the relation to the picture in
			// /xl/drawings/_rels/vmlDrawing1.vml.rels
			XSSFPictureData picData = sheet.getWorkbook().getXSSFWorkbook().getAllPictures().get(pictureIdx);
			String rIdPic = vmldrawing.addRelation(null, XSSFRelation.IMAGES, picData).getRelationship().getId();
			// get image dimension
			ByteArrayInputStream is = new ByteArrayInputStream(picData.getData());
			// setting the image width = 3cm and height = 1.5 cm in pixels
			java.awt.Dimension imageDimension = new java.awt.Dimension(162, 56);
			is.close();
			// updating the VmlDrawing
			vmldrawing.setRelationIdPic(rIdPic);
			vmldrawing.setPictureTitle(pictureTitle);
			vmldrawing.setImageDimension(imageDimension);
			vmldrawing.setHeaderPosition(headerPos);
			// creating the relation to /xl/drawings/vmlDrawing1.xml in
			String rIdExtLink = sheet.getWorkbook().getXSSFWorkbook().getSheet(sheetName).addRelation(null, XSSFRelation.VML_DRAWINGS, vmldrawing).getRelationship().getId();
			sheet.getWorkbook().getXSSFWorkbook().getSheet(sheetName).getCTWorksheet().addNewLegacyDrawingHF().setId(rIdExtLink);
		} catch (Exception e) {
			logger.info("Error Occured in createPictureForHeader : {}", e.getMessage());
		}
	}

	@Override
	public String getNumberOfRecords(ReportVO vo) {
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		String query = generateQuery(vo.getSelectedFields(), vo.getReportView(), vo.getConditions(), vo.getPersonId(), vo.getReportTypeId(), vo.getJoinWhereClause(), false);
		if (query != null) {
			query = queryCountOfRecords(query);
			List<HashMap<String, Object>> result = reportDao.generateReportByQuery(query);
			Integer count = ((result != null && !result.isEmpty() && result.get(0).get("COUNT") != null) ? Integer.parseInt(result.get(0).get("COUNT").toString()): null);
			vo.setTotalRecords(count);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private String queryCountOfRecords(String query) {
		return "SELECT COUNT(*) AS COUNT FROM (" + query + ") TAB";
	}

	@Override
	public String deleteReportTemplate(ReportVO vo) {
		ReportTemplate reportTemplate = reportDao.getReportTemplateById(vo.getReportTemplateId());
		vo.setMessage(reportDao.deleteReportTemplate(reportTemplate));
		return commonDao.convertObjectToJSON(vo);
	}

	public ResponseEntity<byte[]> getResponseEntityForDownload(CommonVO vo, SXSSFWorkbook workbook) throws Exception {
		byte[] byteArray = null;
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		try {
			String exportType = vo.getExportType();
			String documentHeading = vo.getDocumentHeading();
			logger.info("exportType : {}", exportType);
			logger.info("documentHeading : {}", documentHeading);
			workbook.write(bos);
			byteArray = bos.toByteArray();
			return getResponseEntity(byteArray);
		} catch (Exception e) {
			logger.error("Error occured in getResponseEntityForDownload : {}", e.getMessage());
		} finally {
			bos.close();
		}
		return null;
	}

	private ResponseEntity<byte[]> getResponseEntity(byte[] bytes) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.parseMediaType("application/octet-stream"));
			headers.setContentLength(bytes.length);
			headers.setCacheControl("must-revalidate, post-check=0, pre-check=0");
			headers.setPragma("public");
			attachmentData = new ResponseEntity<>(bytes, headers, HttpStatus.OK);
		} catch (Exception e) {
			logger.error("Error in method getResponseEntity", e);
		}
		return attachmentData;
	}

	private String getDecryptedSecretKeyAES() throws Exception {
		return excelityService.decryptAESKey(excelityService.getSftpConfigurationValueAsString(Constants.AES_SECRETE_KEY));
	}

	private String decryptAESData(String data, String secretKey) throws Exception {
		try {
			byte[] iv = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
			IvParameterSpec ivspec = new IvParameterSpec(iv);
			SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256");
			KeySpec spec = new PBEKeySpec(secretKey.toCharArray(), saltValue.getBytes(), 65536, 256);
			SecretKey tmp = factory.generateSecret(spec);
			SecretKeySpec secKey = new SecretKeySpec(tmp.getEncoded(), "AES");
			Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING");
			cipher.init(Cipher.DECRYPT_MODE, secKey, ivspec);
			return new String(cipher.doFinal(Base64.getDecoder().decode(data)));
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Error while AES Data decrypting:", e.getMessage());
		}
		return null;
	}

	@Override
	public XSSFWorkbook getXSSFWorkbookForAdministrativeReport(CommonVO vo) {
		XSSFWorkbook workbook = new XSSFWorkbook();
		AuditReportType auditReportType = auditReportTypeRepository.findAuditReportTypeByReportType(vo.getType());
		vo.setDocumentHeading(auditReportType != null ? auditReportType.getReportName() : "Audit Report");
		XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading());
		commonService.addDetailsInHeader(workbook, sheet);
		List<Object[]> reportData = reportDao.administrativeDetails(vo);
		Object[] headers = reportData.get(0);
		reportData.remove(0);
		dashboardService.prepareExcelSheet(reportData, sheet, headers, workbook, vo);
		return workbook;
	}

	@Override
	public String getAuditReportTypes() {
		Iterable<AuditReportType> data = auditReportTypeRepository.findAll();
		return commonDao.convertObjectToJSON(data);
	}
	
	@Override
	public void roleRightAuditReport() {
		CommonVO vo = new CommonVO();
		vo.setType(Constants.MANPOWER_VIEW_BASE_SALARY_RIGHT_AUDIT_REPORT);
		XSSFWorkbook workbook = new XSSFWorkbook();
		vo.setDocumentHeading("Manpower view base salary audit report - " + formatDate());
		XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading());
		commonService.addDetailsInHeader(workbook, sheet);
		List<Object[]> reportData = reportDao.administrativeDetails(vo);
		Object[] headers = reportData.get(0);
		reportData.remove(0);
		dashboardService.prepareExcelSheet(reportData, sheet, headers, workbook, vo);
		StringBuilder emailBody = new StringBuilder().append("Dear All, <br/> Attached the list of users who have been assigned role with ''MANPOWER_VIEW_BASE_SALARY'' right on  ")
				.append(formatDate());
		sendRoleRightAuditReport(workbook, emailBody);
	}

	private void sendRoleRightAuditReport(XSSFWorkbook workbook, StringBuilder emailBody) {
		File report = commonService.createfileInUploads(workbook, "AUDIT_REPORT_MANPOWER_VIEW_BASE_SALARY.xlsx");
        EmailServiceVO emailServiceVO = new EmailServiceVO();
        emailServiceVO.setBody(emailBody.toString());
        emailServiceVO.setSubject(new StringBuilder("Person role audit report for MANPOWER_VIEW_BASE_SALARY right : ")
				.append(formatDate()).toString());
        emailServiceVO.setFileName(report);
        Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
        NotificationRecipient notificationRecipient = new NotificationRecipient();
        notificationRecipient.setRoleTypeCode(Constants.MANPOWER_VIEW_BASE_SALARY_RIGHT_AUDIT_REPORT_EMAIL_ROLE);
        notificationRecipient.setRecipientType( Constants.NOTIFICATION_RECIPIENT_TYPE_TO);
        dynamicEmailRecipients.add(notificationRecipient);
        emailServiceVO.setModuleCode(0);
        emailServiceVO.setSubModuleCode(Constants.SUBMODULE_CODE.toString());
        emailServiceVO.setRecipients(dynamicEmailRecipients);
        emailService.sendEmail(emailServiceVO);
	}

	private String formatDate() {
		try {
			DateTimeFormatter formatter = DateTimeFormatter.ofPattern(Constants.REQUIRED_DATE_FORMAT);
			return LocalDate.now(ZoneId.of(Constants.CRON_JOB_TIMEZONE)).minusDays(1).format(formatter);
		} catch (Exception e) {
			logger.error("Error while formatDate {}", e.getMessage());
		}
		return null;
	}

	@Override
	public void generateReportFromBirt(ReportVO vo, HttpServletRequest request, HttpServletResponse response) {
		try {
			RestTemplate restTemplate = new RestTemplate();
			ReqObject reqObject = new ReqObject();
			reqObject.setReportTypeId(vo.getReportTypeId());
			reqObject.setExportType(vo.getExportType());
			reqObject.setInputParams(vo.getInputPrams());
			HttpHeaders headers = new HttpHeaders();
			headers.add(Constants.HEADER_STRING, request.getHeader(Constants.HEADER_STRING));
			HttpEntity<ReqObject> entity = new HttpEntity<>(reqObject, headers);
			String url = new StringBuilder(birtUrl).append("generateReportFromBirt").toString();
			String birtResponse = restTemplate.postForObject(url, entity, String.class);
			if ("You are not authorized to open this resource".equals(birtResponse)) {
				throw new ApplicationException("You are not authorized to open this resource", Constants.JAVA_ERROR);
			}
			if (birtResponse != null) {
				response.getOutputStream().print(birtResponse);
			}
		} catch (ResourceAccessException e) {
			throw new ApplicationException("BIRT Engine is out of service", e, Constants.JAVA_ERROR);
		} catch (ApplicationException e) {
			throw e;
		} catch (Exception e) {
			throw new ApplicationException("Exception occured in BIRT Engine Service", e, Constants.JAVA_ERROR);
		}
	}

	@Override
	public List<Field> getParameterDetails(String reportTypeId, String token) {
		ReqObject reqObject = new ReqObject();
		try {
			RestTemplate restTemplate = new RestTemplate();
			reqObject.setReportTypeId(reportTypeId);
			List<ReportColumns> columns = reportDao.getReportColumnsByType(reportTypeId);
			reqObject.setColumns(columns);
			HttpHeaders headers = new HttpHeaders();
			headers.add(Constants.HEADER_STRING, token);
			HttpEntity<ReqObject> entity = new HttpEntity<>(reqObject, headers);
			String url = new StringBuilder(birtUrl).append("getParameterDetails").toString();
			String birtResponse = restTemplate.postForObject(url, entity, String.class);
			ObjectMapper mapper = new ObjectMapper();
			reqObject = mapper.readValue(birtResponse, ReqObject.class);
		} catch (ResourceAccessException e) {
			throw new ApplicationException("BIRT Engine is out of service", e, Constants.JAVA_ERROR);
		} catch (Exception e) {
			throw new ApplicationException("Exception occured in BIRT Engine Service", e, Constants.JAVA_ERROR);
		}
		return reqObject.getFields();
	}

}
