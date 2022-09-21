package com.polus.fibicomp.claims.claimsIntegration.sapfeed.service;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.Vector;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.transaction.Transactional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFFont;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.ChannelSftp.LsEntry;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.SftpException;
import com.polus.fibicomp.claims.claimsIntegration.excelity.service.SftpConfigurationService;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.dao.ClaimInvoiceIntegrationDao;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeed;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeedResponseMessage;
import com.polus.fibicomp.claims.dao.ClaimsDao;
import com.polus.fibicomp.claims.pojo.Claim;
import com.polus.fibicomp.claims.pojo.ClaimInvoiceLog;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.fastintegration.sapfeedmaintenance.vo.SapFeedMaintenanceVO;
import com.polus.fibicomp.fastintegration.service.FastIntegrationService;
import com.polus.fibicomp.fastintegration.vo.IntegrationReportVO;
import com.polus.fibicomp.fastintegration.vo.IntegrationReportVO.EmailContent;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;

@Transactional
@Service(value = "claimInvoiceIntegrationService")
public class ClaimInvoiceIntegrationServiceImpl implements ClaimInvoiceIntegrationService {

    @Value("${system.timezone}")
    private String timezone;

    @Value("${fast.sftp.password}")
    private String fastSftpPassword;
    
    @Autowired
    private FastIntegrationService fastIntegrationService;
    
    @Autowired
    private SftpConfigurationService sftpConfigurationService;

    @Autowired
    private CommonDao commonDao;
    
    @Autowired
    private ClaimInvoiceIntegrationDao claimInvoiceIntegrationDao;
    
    @Autowired
    private ClaimInvoiceFeedMaintenanceService claimInvoiceFeedMaintenanceService;

    @Autowired
    private EmailService emailService;

	@Autowired
	private ClaimsDao claimDao;

    @Autowired
    private CommonService commonService;

    protected static Logger logger = LogManager.getLogger(ClaimInvoiceIntegrationServiceImpl.class.getName());
    private static final String SUCCESS = "SUCCESS";
    private static final String FAILED = "FAILED";
    private static final String AWARD_NUMBER = ",  Award Number : ";
	private static final String ACCOUNT_NUMBER = ",  Account Number : " ;
	private static final String EMAILBODY_STRUCTURE= "Please go to " ;
	private static final String EMAILBODY_FILE_LOC = "  to view the files<br/>" ;
	private static final String CLAIM_NUMBER = "Claim Number:  ";
    @Override
    public String fastTemplateGeneration(List<Integer> feedIds, SapFeedMaintenanceVO sapFeedMaintenanceVO) {
    	IntegrationReportVO vo = new IntegrationReportVO();
        try {
        	Boolean checkSFTPStatus = commonService.checkSFTPConnection(vo.getEmailContent());
        	if (Boolean.TRUE.equals(checkSFTPStatus)) {
        		List<String> newFeedIds = feedIds != null  && !feedIds.isEmpty() ? feedIds.stream().map(Object::toString).collect(Collectors.toList()) : new ArrayList<>();   
            	Integer batchId = claimInvoiceIntegrationDao.generateInvoiceBatchId(newFeedIds, vo);
                logger.info("Batch ID generated {}", batchId);
            	prepareClaimSapFeedFiles(vo, batchId);   
    		    if (sapFeedMaintenanceVO != null) {
    		    	sapFeedMaintenanceVO.setMessage(SUCCESS);
    		    	claimInvoiceFeedMaintenanceService.getClaimBatchDashboard(sapFeedMaintenanceVO);
    		    }
    		    Set<NotificationRecipient> dynamicRecipients = fetchDynamicEmailRecipients();
    		    if (batchId != null) {
    		    	List<SapClaimFeed> sapClaimFeeds = claimInvoiceIntegrationDao.getClaimInvoiceBatchByBatchId(batchId);
    		    	List<Integer> sapClaimCount = new ArrayList<>();
    		    	sapClaimCount.add(sapClaimFeeds.size());
    		        vo.setEmailContent(setFileEmailBody(batchId, sapClaimFeeds, vo.getEmailContent(), vo.getIsResponseMail(), sapClaimCount));
    		        sendSapClaimFeedNotification(Boolean.TRUE, vo, Constants.SAP_CLAIM_FEED_INTERFACE_GEN_NOTIFICATION_CODE, dynamicRecipients);
    		    }
    		    if (vo.getEmailContent() != null && vo.getEmailContent().getError().length() > 0) {
		        	sendSapClaimFeedNotification(Boolean.FALSE, vo, Constants.SAP_CLAIM_FEED_INTERFACE_GEN_ERR_NOTIFICATION_CODE, dynamicRecipients);
		        }
        	} else {
                sendErrorReportMail(vo, Constants.SAP_CLAIM_FEED_INTERFACE_GEN_ERR_NOTIFICATION_CODE);
        	}
        } catch (Exception e) {
            logger.error("Error in fast execution claimInvoiceSapIntegration {}", e.getMessage());
            fastIntegrationService.createFastIntegrationLog("Error in fast execution claimInvoiceSapIntegration {}" + e);
            e.printStackTrace();
            if (sapFeedMaintenanceVO != null) {
            	sapFeedMaintenanceVO.setMessage(FAILED);
            }
            vo.getEmailContent().getError().append("Error in claimInvoiceSapIntegration method :").append(e).append("<br/>");
            sendErrorReportMail(vo, Constants.SAP_CLAIM_FEED_INTERFACE_GEN_ERR_NOTIFICATION_CODE);
        }
		logger.info("claimInvoiceSapIntegration ended");
		return commonDao.convertObjectToJSON(sapFeedMaintenanceVO);
    }

    private Map<String, String> getSapFeedClaimPlaceholders(Boolean successOrErrorFlag, IntegrationReportVO vo) {			
		Map<String, String> placeHolder = new HashMap<>();
		if (Boolean.TRUE.equals(successOrErrorFlag)) {
			placeHolder.put("{CLAIM_DETAIL}", vo.getEmailContent().getSuccess().toString());
		} else {
			placeHolder.put("{CLAIM_DETAIL}", vo.getEmailContent().getError().toString());
		}
		DateFormat dateFormat = new SimpleDateFormat(Constants.TWELVE_HOUR_DATE_FORMAT);
		dateFormat.setTimeZone(TimeZone.getTimeZone(Constants.CRON_JOB_TIMEZONE));
		placeHolder.put("{CURRENT_TIMESTAMP}", dateFormat.format(commonDao.getCurrentTimestamp()));
		return placeHolder;
	}

    private void sendSapClaimFeedNotification(Boolean successOrErrorFlag, IntegrationReportVO vo, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailRecipients) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(notificationTypeId);
		emailServiceVO.setModuleCode(Constants.CLAIM_MODULE_CODE);
		emailServiceVO.setPlaceHolder(getSapFeedClaimPlaceholders(successOrErrorFlag, vo));
		emailServiceVO.setSubModuleCode(Constants.CLAIM_SUBMODULE_CODE.toString());
		if (dynamicEmailRecipients != null && !dynamicEmailRecipients.isEmpty()) {
			emailServiceVO.setRecipients(dynamicEmailRecipients);
		}
		emailService.sendEmail(emailServiceVO);
		if (vo.getEmailContent().getSuccess().length() == 0) {
            vo.setEmailContent(vo.new EmailContent());
        }
	}

    private EmailContent setFileEmailBody(Integer batchId, List<SapClaimFeed> sapClaimFeeds, EmailContent emailBody,	Boolean isResponseMail, List<Integer> sapClaimCount) {
    	emailBody.getSuccess().append("</b><br/>").append("Integration file status Report for Batch ID : ").append(batchId.toString()).append( "  ").append(commonDao.getDateFromTimestampZoneFormat(Constants.CRON_JOB_TIMEZONE, Constants.LONG_DATE_FORMAT)).append("</b><br/>");
		emailBody.getSuccess().append("Total claim sent in this Batch :   ").append(sapClaimCount.get(0).toString()).append("<br/>");
		if (Boolean.TRUE.equals(isResponseMail)) {
		emailBody.getSuccess().append("Total Successful Interface in this Batch:   ").append(sapClaimCount.get(1).toString()).append("<br/>");
		emailBody.getSuccess().append("Total Error in interface in this Batch:    ").append(sapClaimCount.get(2).toString()).append("<br/>");
		emailBody.getSuccess().append("No response recorded in this Batch: ").append(sapClaimCount.get(3).toString()).append("<br/>");
		}
		if (Boolean.FALSE.equals(isResponseMail)) {
			emailBody.getSuccess().append(EMAILBODY_STRUCTURE).append(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.RISE_CLAIM_INVOICE_INBOUND)).append(EMAILBODY_FILE_LOC);
			emailBody.getSuccess().append("<br/> <b>List of claim in pending stage</b> <br/>");
			for (SapClaimFeed sapClaimFeed : sapClaimFeeds) {
				if (sapClaimFeed.getFeedStatus().equals("F")) {
					Claim claim = claimDao.getClaim(sapClaimFeed.getClaimId());
					if (claim != null) {
						emailBody.getSuccess().append(CLAIM_NUMBER).append(sapClaimFeed.getClaimNumber()).append(AWARD_NUMBER).append(claim.getAwardNumber() != null ? claim.getAwardNumber() : " ").append(ACCOUNT_NUMBER).append(claim.getAccountNumber() != null ? claim.getAccountNumber() : "").append("<br/><br/>");
					}
				}
			}
		} else {
			emailBody.getSuccess().append(EMAILBODY_STRUCTURE).append(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.RISE_CLAIM_INVOICE_OUTBOUND)).append(EMAILBODY_FILE_LOC);
			emailBody.getSuccess().append("<br/> <b>List of Claim successfully interfaced</b> <br/>");
			for (SapClaimFeed sapClaimFeed : sapClaimFeeds) {
				if (sapClaimFeed.getFeedStatus().equals("R")) {
					Claim claim = claimDao.getClaim(sapClaimFeed.getClaimId());
					if(claim != null) {
						emailBody.getSuccess().append(CLAIM_NUMBER).append(sapClaimFeed.getClaimNumber()).append(AWARD_NUMBER).append(claim.getAwardNumber() != null ? claim.getAwardNumber() : " ").append(ACCOUNT_NUMBER).append(claim.getAccountNumber() != null ? claim.getAccountNumber() : "").append("<br/><br/>");
					}
				}
			}
			emailBody.getSuccess().append("<br/> <b>List of Claim which got error in response</b> <br/>");
			for (SapClaimFeed sapClaimFeed : sapClaimFeeds) {
				if (sapClaimFeed.getFeedStatus().equals("E")) {
					Claim claim = claimDao.getClaim(sapClaimFeed.getClaimId());
					if(claim != null) {
						emailBody.getError().append(CLAIM_NUMBER).append(sapClaimFeed.getClaimNumber()).append(AWARD_NUMBER).append(claim.getAwardNumber() != null ? claim.getAwardNumber() : " ").append(ACCOUNT_NUMBER).append(claim.getAccountNumber() != null ? claim.getAccountNumber() : "").append("<br/><br/>");
					}
				}
			}
			emailBody.getSuccess().append("<br/> <b>List of claim without any response</b><br/>");
			for (SapClaimFeed sapClaimFeed : sapClaimFeeds) {
				if (sapClaimFeed.getFeedStatus().equals("F")) {
					Claim claim = claimDao.getClaim(sapClaimFeed.getClaimId());
					if(claim != null) {
						emailBody.getSuccess().append(CLAIM_NUMBER).append(sapClaimFeed.getClaimNumber()).append(AWARD_NUMBER).append(claim.getAwardNumber() != null ? claim.getAwardNumber() : " ").append(ACCOUNT_NUMBER).append(claim.getAccountNumber()).append("<br/><br/>");
					}
				}
			}
		}
		emailBody.getSuccess().append("<br/><b>Note: Successful interface of award is determined solely based on 'S' response received on all interface files.</b></br>");
		if (emailBody.getError().length() != 0) {
			emailBody.getError().append("<br/><b>Note: Successful interface of award is determined solely based on 'S' response received on all interface files.</b></br>");
		}
		return emailBody;
	}

	public IntegrationReportVO prepareClaimSapFeedFiles(IntegrationReportVO vo, Integer batchId) {
        try { 	
            String sapFeedFileName = "rise_claim_invoice";        
            List<ClaimInvoiceLog> claimInvoiceLog = claimInvoiceIntegrationDao.getClaimInvoiceLogByBatchId(batchId);    
        	claimInvoiceLogToExcel(sapFeedFileName, claimInvoiceLog, batchId, vo.getEmailContent());
            if (claimInvoiceLog != null && !claimInvoiceLog.isEmpty()) {
            	vo.setClaimInvoiceCount(claimInvoiceLog.size());
            	fastIntegrationService.createFastIntegrationLog("Total Claim Invoice data count :" + claimInvoiceLog.size());
            } else {
                vo.setClaimInvoiceCount(0);
            }                  
        } catch (Exception e) {
            logger.info("Error in fast execution prepareClaimSapFeedFiles {}", e.getMessage());
            fastIntegrationService.createFastIntegrationLog("Error in fast execution prepareClaimSapFeedFiles {}" + e);
            e.printStackTrace();
            vo.getEmailContent().getError().append("Exception while processing files in prepareClaimSapFeedFiles : ").append(e).append("<br/>");
        }
        return vo;
    }

	private String claimInvoiceLogToExcel(String sapFeedFileName, List<ClaimInvoiceLog> claimInvoiceLog, Integer batchId, EmailContent emailContent) {
    	try (XSSFWorkbook workbook = new XSSFWorkbook()) {
            XSSFSheet sheet = workbook.createSheet("Claim Invoice");
            createClaimSapFeedReportByParams(claimInvoiceLog, batchId, sheet, workbook); 
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            workbook.write(bos);          
            byte[] byteArray = null;
            byteArray = bos.toByteArray();
            byteArray = convertXlsxToCsvFile(byteArray);
            File file = createFile(sapFeedFileName, batchId, emailContent);
            try (OutputStream os = new FileOutputStream(file)){
            	 os.write(byteArray);
			} catch (Exception e) {
	            logger.info("error occured while writing file to local storage {} ", e.getMessage());
	            emailContent.getError().append("Error occured while writing file to local storage: ").append(e).append("<br/>");
			}
            String newFileName = rename(file);
            if (commonDao.getParameterValueAsBoolean(Constants.IS_SFTP_ENABLED)) {
                File newFile =  new File(newFileName);
            	String sftpPath = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_INVOICE_INBOUND_FILE_PATH);
                fastIntegrationService.movetoSftp(newFile.getParent() + "/" + newFile.getName(), sftpPath, emailContent);
            }
        } catch (Exception e) {
            logger.info("Error in claimInvoiceLogToExcel", e.getMessage());
            emailContent.getError().append("Error in claimInvoiceLogToExcel").append(e).append("<br/>");
            e.printStackTrace();
        }
		return null;		
	}
	
	private String rename(File file) {
        String files = file.getAbsolutePath();
        String newfiles = files.substring(0, files.lastIndexOf("."));
        newfiles = newfiles + ".csv";
        String str = newfiles;
        if (file.renameTo(new File(str))) {
            return str;
        } else {
            return files;
        }
	}
	
	private byte[] convertXlsxToCsvFile(byte[] data) {
		StringBuilder byteArray = new StringBuilder();
		InputStream inputStream = new ByteArrayInputStream(data);
		try (Workbook workbook = WorkbookFactory.create(inputStream)) {
			Sheet sheet = workbook.getSheetAt(0);
			Row row = null;
			for (int i = 0; i < sheet.getLastRowNum() + 1; i++) {
				row = sheet.getRow(i);
				StringBuilder rowDetail = new StringBuilder();
				if (sheet.getRow(i).getPhysicalNumberOfCells() > 0) {
					for (int j = 0; j < sheet.getRow(i).getPhysicalNumberOfCells(); j++) {
						String newData = null;
						if(row.getCell(j) != null)
							newData = escapeSpecialCharactersFromData(row.getCell(j).toString());
						if (i == 0 && newData != null) {
							rowDetail = rowDetail.append(newData).append(",");
						} else {
							if (row.getCell(j) == null) {
								rowDetail = rowDetail.append('"').append('"').append(',');
							} else {
								rowDetail = rowDetail.append(newData).append(",");
							}
						}
					}
					byteArray = byteArray.append(rowDetail.substring(0, rowDetail.length() - 1)).append("\n");
				}
			}
			workbook.close();
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Exception in convertXlsxToCsvFile : {}", e.getMessage());
		}
		return byteArray.toString().getBytes(StandardCharsets.UTF_8);
	}

	public String escapeSpecialCharactersFromData(String data) {
	    String escapedData = data.replaceAll("\\R", " ");
	    escapedData =  escapedData.replaceAll("[\\r\\n]+", " ");
	    if (escapedData.contains(",") || escapedData.contains("\"") || escapedData.contains("'") || escapedData.equals("")) {
	    	escapedData = escapedData.replace("\"", "\"\"");
	        escapedData = "\"" + escapedData + "\"";
	    }
	    return escapedData;
	}
	
	 public File createFile(String fileName, Integer batchId, EmailContent emailContent) {
        BufferedWriter fileOutputStream = null;
        try {
        	String filepath = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.RISE_CLAIM_INVOICE_INBOUND);
            String fileNameWithPath = new StringBuilder(filepath).append(File.separator).append(fileName).append("_").append(batchId.toString()).append("_").append(createdDateWithTime()).append(".tmp").toString();
            File file = new File(fileNameWithPath);
            fileOutputStream = new BufferedWriter(new FileWriter(fileNameWithPath));
            fileOutputStream.close();
            return file;
        } catch (Exception e) {
            logger.error("Exception in method createFile : {} ", e);
            emailContent.getError().append("Error in creating file : ").append(e).append("<br/>");
            return null;
        } finally {
            try {
				if(fileOutputStream != null) {
					fileOutputStream.close();
				}
            } catch (IOException e) {
                logger.error("Exception in method createFile while closing the stream : {} ", e);
            }
        }
	 }
	 
	private String createdDateWithTime() {
	    String date = convertDateFormatBasedOnTimeZone(commonDao.getCurrentTimestamp().getTime(),
	            Constants.FAST_INTEGRATION_LOG_DATE_FORMAT);
	    SimpleDateFormat time = new SimpleDateFormat("HHmmss");
	    time.setTimeZone(TimeZone.getTimeZone(timezone));
	    String createdTime = time.format(new Date());
	    return date + createdTime;
	}

	private String convertDateFormatBasedOnTimeZone(Long dateValue, String dateFormat) {
        Date date = new Date(dateValue);
        return new SimpleDateFormat(dateFormat).format(commonDao.adjustTimezone(date));
	}
	 
	private XSSFSheet createClaimSapFeedReportByParams(List<ClaimInvoiceLog> claimInvoiceLog, Integer batchId, XSSFSheet sheet,
			XSSFWorkbook workbook) {
        Object[] tableHeadingRows = {"Indicator", "Reference", "Doc Header Text", "Currency code", "Document date",
                "Document type", "Customer Email Address", "Requester Email Address", "Base date", "Invoice/Credit Memo", "Company Code",
                "SAP Doc Nbr", "Fiscal Year", "Action Indicator", "Particulars1", "Particulars2", "Particulars3", "Particulars4", "Contact/Telephone", "RISE Invoice ID", "Batch ID"};
        Object[] tableHeading2Rows = {"Indicator", "Reference", "Grant Code", "Posting Key", "WBS",
                "Profit centre", "Business Area", "GL account", "Tax Code", "Amount(Exclude Tax Amount)Document Currency", "Assignment field", "Text field","Customer Number","","","","","","","",""};
        prepareExcelSheet(claimInvoiceLog, sheet, tableHeadingRows,tableHeading2Rows, workbook);
        return sheet;
		
	}

	private void prepareExcelSheet(List<ClaimInvoiceLog> claimInvoiceLog, XSSFSheet sapFeedSheet,
			Object[] tableHeadingRows,Object[] tableHeading2Rows, XSSFWorkbook workbook) {
        XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
        int rowNumber = 0;
		prepareExcelSheetHeader(rowNumber,sapFeedSheet, tableHeadingRows, tableHeading2Rows, null, workbook, tableBodyStyle);
		prepareExcelSheetData(++rowNumber, sapFeedSheet,workbook, claimInvoiceLog);
		autoSizeColumns(workbook);		
	}
	
	private void autoSizeColumns(XSSFWorkbook workbook) {
		int numberOfSheets = workbook.getNumberOfSheets();
		for (int i = 0; i < numberOfSheets; i++) {
			XSSFSheet sheet = workbook.getSheetAt(i);
			if (sheet.getPhysicalNumberOfRows() > 0) {
				Row row = sheet.getRow(1);
				Iterator<Cell> cellIterator = row.cellIterator();
				while (cellIterator.hasNext()) {
					Cell cell = cellIterator.next();
					int columnIndex = cell.getColumnIndex();
					sheet.autoSizeColumn(columnIndex);
				}
			}
		}
	}
	
	 private void prepareExcelSheetData(int rowNumber, XSSFSheet sheet, XSSFWorkbook workbook, List<ClaimInvoiceLog> claimInvoiceLog) {
		// Table body style and font creation code.
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		tableBodyStyle.setBorderTop(BorderStyle.HAIR);
		tableBodyStyle.setBorderBottom(BorderStyle.HAIR);
		tableBodyStyle.setBorderLeft(BorderStyle.HAIR);
		tableBodyStyle.setBorderRight(BorderStyle.HAIR);
		tableBodyStyle.setWrapText(true);
		XSSFFont tableBodyFont = workbook.createFont();
		tableBodyFont.setFontHeightInPoints((short) 12);
		tableBodyStyle.setFont(tableBodyFont);
		AtomicInteger rowNumberCount = new AtomicInteger(++rowNumber);
		claimInvoiceLog.forEach(invoice -> {
			String grantCode = invoice.getGrantCode();
			String profitCentre = "";
			String baCode = "";
			BigDecimal amount = BigDecimal.ZERO;
			Row headerRow1 = sheet.createRow(rowNumberCount.getAndIncrement());
			Cell indicator = headerRow1.createCell(0);
			indicator.setCellStyle(tableBodyStyle);
			indicator.setCellValue("H");
			Cell claimNumber = headerRow1.createCell(1);
			claimNumber.setCellStyle(tableBodyStyle);
			claimNumber.setCellValue(invoice.getClaimNumber());
			Cell docHeader = headerRow1.createCell(2);
			docHeader.setCellStyle(tableBodyStyle);
			docHeader.setCellValue(invoice.getDocumentHeaderText());
			Cell currency = headerRow1.createCell(3);
			currency.setCellStyle(tableBodyStyle);
			currency.setCellValue(invoice.getCurrencyCode());
			Cell docDate = headerRow1.createCell(4);
			docDate.setCellStyle(tableBodyStyle);
			docDate.setCellValue("");
			Cell docType = headerRow1.createCell(5);
			docType.setCellStyle(tableBodyStyle);
			docType.setCellValue(invoice.getDocumentTypeCode());
			Cell customerEmail = headerRow1.createCell(6);
			customerEmail.setCellStyle(tableBodyStyle);
			customerEmail.setCellValue(invoice.getCustomerEmailAddress());
			Cell requesterEmail = headerRow1.createCell(7);
			requesterEmail.setCellStyle(tableBodyStyle);
			requesterEmail.setCellValue(invoice.getRequesterEmailAddress());
			Cell baseDate = headerRow1.createCell(8);
			baseDate.setCellStyle(tableBodyStyle);
			baseDate.setCellValue("");
			Cell memo = headerRow1.createCell(9);
			memo.setCellStyle(tableBodyStyle);
			memo.setCellValue(invoice.getClaimInvoiceFeedType().getDescription());			
			Cell companyCode = headerRow1.createCell(10);
			companyCode.setCellStyle(tableBodyStyle);
			companyCode.setCellValue(invoice.getCompanyCode());
			Cell sapDocNumber = headerRow1.createCell(11);
			sapDocNumber.setCellStyle(tableBodyStyle);
			sapDocNumber.setCellValue(invoice.getInputDocumentNumber());
			Cell fiscalYear = headerRow1.createCell(12);
			fiscalYear.setCellStyle(tableBodyStyle);
			fiscalYear.setCellValue(invoice.getFiscalYear());
			Cell actionIndicator = headerRow1.createCell(13);
			actionIndicator.setCellStyle(tableBodyStyle);
			actionIndicator.setCellValue("C");
			Cell part1 = headerRow1.createCell(14);
			part1.setCellStyle(tableBodyStyle);
			part1.setCellValue(invoice.getParticulars1());
			Cell part2 = headerRow1.createCell(15);
			part2.setCellStyle(tableBodyStyle);
			part2.setCellValue(invoice.getParticulars2());
			Cell part3 = headerRow1.createCell(16);
			part3.setCellStyle(tableBodyStyle);
			part3.setCellValue(invoice.getParticulars3());
			Cell part4 = headerRow1.createCell(17);
			part4.setCellStyle(tableBodyStyle);
			part4.setCellValue(invoice.getParticulars4());			
			Cell telephone = headerRow1.createCell(18);
			telephone.setCellStyle(tableBodyStyle);
			telephone.setCellValue(invoice.getContactTelephoneNo());
			Cell invoiceId = headerRow1.createCell(19);
			invoiceId.setCellStyle(tableBodyStyle);
			invoiceId.setCellValue(invoice.getClaimInvoiceLogId().toString());			
			Cell batchId = headerRow1.createCell(20);
			batchId.setCellStyle(tableBodyStyle);
			batchId.setCellValue(invoice.getBatchId().toString());
			
			Row headerRow2 = sheet.createRow(rowNumberCount.getAndIncrement());
			Cell indicator2 = headerRow2.createCell(0);
			indicator2.setCellStyle(tableBodyStyle);
			indicator2.setCellValue("I");
			Cell claimNumber1 = headerRow2.createCell(1);
			claimNumber1.setCellStyle(tableBodyStyle);
			claimNumber1.setCellValue(invoice.getClaimNumber());
			Cell grantCodeCell = headerRow2.createCell(2);
			grantCodeCell.setCellStyle(tableBodyStyle);
			grantCodeCell.setCellValue(grantCode);
			Cell postingKey = headerRow2.createCell(3);
			postingKey.setCellStyle(tableBodyStyle);
			postingKey.setCellValue(invoice.getHeaderPostingKey());
			Cell wbsCell = headerRow2.createCell(4);
			wbsCell.setCellStyle(tableBodyStyle);
			wbsCell.setCellValue("");
			Cell profitCentre2 = headerRow2.createCell(5);
			profitCentre2.setCellStyle(tableBodyStyle);
			profitCentre2.setCellValue(profitCentre);
			Cell baCodeCell = headerRow2.createCell(6);
			baCodeCell.setCellStyle(tableBodyStyle);
			baCodeCell.setCellValue(baCode);
			Cell glAccount = headerRow2.createCell(7);
			glAccount.setCellStyle(tableBodyStyle);
			glAccount.setCellValue(invoice.getGlAccountCode());
			Cell dateValue = headerRow2.createCell(8);
			dateValue.setCellStyle(tableBodyStyle);
			dateValue.setCellValue("11");
			Cell assigment = headerRow2.createCell(10);
			assigment.setCellStyle(tableBodyStyle);
			assigment.setCellValue(invoice.getAssignmentField());
			Cell desc = headerRow2.createCell(11);
			desc.setCellStyle(tableBodyStyle);
			desc.setCellValue(invoice.getDescription());
			Cell customerNumber = headerRow2.createCell(12);
			customerNumber.setCellStyle(tableBodyStyle);
			customerNumber.setCellValue(invoice.getCustomerNumber() != null? invoice.getCustomerNumber(): "");
			Cell emptyactionIndicator = headerRow2.createCell(13);
			emptyactionIndicator.setCellStyle(tableBodyStyle);
			emptyactionIndicator.setCellValue("");
			Cell emptypart1 = headerRow2.createCell(14);
			emptypart1.setCellStyle(tableBodyStyle);
			emptypart1.setCellValue("");
			Cell emptypart2 = headerRow2.createCell(15);
			emptypart2.setCellStyle(tableBodyStyle);
			emptypart2.setCellValue("");
			Cell emptypart3 = headerRow2.createCell(16);
			emptypart3.setCellStyle(tableBodyStyle);
			emptypart3.setCellValue("");
			Cell emptypart4 = headerRow2.createCell(17);
			emptypart4.setCellStyle(tableBodyStyle);
			emptypart4.setCellValue("");			
			Cell emptytelephone = headerRow2.createCell(18);
			emptytelephone.setCellStyle(tableBodyStyle);
			emptytelephone.setCellValue("");
			Cell emptyinvoiceId = headerRow2.createCell(19);
			emptyinvoiceId.setCellStyle(tableBodyStyle);
			emptyinvoiceId.setCellValue("");			
			Cell emptybatchId = headerRow2.createCell(20);
			emptybatchId.setCellStyle(tableBodyStyle);
			emptybatchId.setCellValue("");
			invoice.getClaimInvoiceDetails().forEach(details -> {
				Row lineItemRow = sheet.createRow(rowNumberCount.getAndIncrement());
				Cell indicatorLine = lineItemRow.createCell(0);
				indicatorLine.setCellStyle(tableBodyStyle);
				indicatorLine.setCellValue("I");
				Cell claimNumberLine = lineItemRow.createCell(1);
				claimNumberLine.setCellStyle(tableBodyStyle);
				claimNumberLine.setCellValue(invoice.getClaimNumber());
				Cell gratnCodeLine = lineItemRow.createCell(2);
				gratnCodeLine.setCellStyle(tableBodyStyle);
				gratnCodeLine.setCellValue(grantCode);
				Cell postingKeyLine = lineItemRow.createCell(3);
				postingKeyLine.setCellStyle(tableBodyStyle);
				postingKeyLine.setCellValue(details.getLineItemPostingKey());
				Cell wbs = lineItemRow.createCell(4);
				wbs.setCellStyle(tableBodyStyle);
				wbs.setCellValue(details.getGrtWbs());
				Cell profitLine = lineItemRow.createCell(5);
				profitLine.setCellStyle(tableBodyStyle);
				profitLine.setCellValue(profitCentre);
				Cell baCodeLine = lineItemRow.createCell(6);
				baCodeLine.setCellStyle(tableBodyStyle);
				baCodeLine.setCellValue(baCode);
				Cell glaccount = lineItemRow.createCell(7);
				glaccount.setCellStyle(tableBodyStyle);
				glaccount.setCellValue(details.getGlAccountCode());
				Cell taxCode = lineItemRow.createCell(8);
				taxCode.setCellStyle(tableBodyStyle);
				taxCode.setCellValue(details.getTaxCode());
				Cell amountLine = lineItemRow.createCell(9);
				amountLine.setCellStyle(tableBodyStyle);
				amountLine.setCellValue(details.getClaimAmount().add(details.getSubContractAmount() == null ? BigDecimal.ZERO : details.getSubContractAmount())
						.setScale(2, RoundingMode.HALF_UP).toPlainString());
				Cell assignmentField = lineItemRow.createCell(10);
				assignmentField.setCellStyle(tableBodyStyle);
				assignmentField.setCellValue("");						
				Cell tetLine = lineItemRow.createCell(11);
				tetLine.setCellStyle(tableBodyStyle);
				tetLine.setCellValue(details.getDescription());
				Cell emptyfiscalYear1 = lineItemRow.createCell(12);
				emptyfiscalYear1.setCellStyle(tableBodyStyle);
				emptyfiscalYear1.setCellValue("");
				Cell emptyactionIndicator1 = lineItemRow.createCell(13);
				emptyactionIndicator1.setCellStyle(tableBodyStyle);
				emptyactionIndicator1.setCellValue("");
				Cell empty1part1 = lineItemRow.createCell(14);
				empty1part1.setCellStyle(tableBodyStyle);
				empty1part1.setCellValue("");
				Cell empty1part2 = lineItemRow.createCell(15);
				empty1part2.setCellStyle(tableBodyStyle);
				empty1part2.setCellValue("");
				Cell empty1part3 = lineItemRow.createCell(16);
				empty1part3.setCellStyle(tableBodyStyle);
				empty1part3.setCellValue("");
				Cell empty1part4 = lineItemRow.createCell(17);
				empty1part4.setCellStyle(tableBodyStyle);
				empty1part4.setCellValue("");			
				Cell emptytelephone1 = lineItemRow.createCell(18);
				emptytelephone1.setCellStyle(tableBodyStyle);
				emptytelephone1.setCellValue("");
				Cell emptyinvoiceId1 = lineItemRow.createCell(19);
				emptyinvoiceId1.setCellStyle(tableBodyStyle);
				emptyinvoiceId1.setCellValue("");			
				Cell emptybatchId1 = lineItemRow.createCell(20);
				emptybatchId1.setCellStyle(tableBodyStyle);
				emptybatchId1.setCellValue("");
			});
			amount = BigDecimal.valueOf(invoice.getClaimInvoiceDetails().stream()
					.mapToDouble(details -> Double.parseDouble(details.getClaimAmount().add(details.getSubContractAmount() == null ? BigDecimal.ZERO : details.getSubContractAmount())
							.setScale(2, RoundingMode.HALF_UP).toString())).sum());
			Cell amountCell = headerRow2.createCell(9);
			amountCell.setCellStyle(tableBodyStyle);
			amountCell.setCellValue(amount.toPlainString());
		});
	}

	private XSSFWorkbook prepareExcelSheetHeader(int rowNumber, XSSFSheet sheet, Object[] tableHeadingRow,Object[] tableHeading2Row, String documentHeading, XSSFWorkbook workbook, XSSFCellStyle tableBodyStyle) {
	        int headingCellNumber = 0;
	        Row tableHeadRow = sheet.createRow(rowNumber++);
	        Cell headingCell = tableHeadRow.createCell(0);
	        headingCell.setCellValue((String) documentHeading);
	        // Set table head data to each column.
	        for (Object heading : tableHeadingRow) {
	            Cell cell = tableHeadRow.createCell(headingCellNumber++);
	            cell.setCellValue((String) heading);
	        }
	        Row tableHead2Row = sheet.createRow(rowNumber++);
	        int heading2CellNumber = 0;
	        for (Object heading : tableHeading2Row) {
	            Cell cell = tableHead2Row.createCell(heading2CellNumber++);
	            cell.setCellValue((String) heading);
	        }
	        return workbook;
	    }

	@Override
	public String processClaimInvoiceFeedResponse(String userActionCode) {
		IntegrationReportVO vo = new IntegrationReportVO();
		vo.setIsResponseMail(Boolean.TRUE);
		String batchId = null;
		Set<Integer> batchIds = new HashSet<>();
		String responsePath = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.RISE_CLAIM_INVOICE_OUTBOUND);
        if (commonDao.getParameterValueAsBoolean(Constants.IS_SFTP_ENABLED)) {
            fastIntegrationService.getSftpResDirectory(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_CLAIM_INVOICE_OUTBOUND), responsePath);
        }
        File directory = new File(responsePath);
        if (directory.isDirectory()) {
            File[] fileData = directory.listFiles();
            if (fileData != null) {              
                for (File file : fileData) {
                    if (file.isFile()) {
                        String fileName = file.getName();
                        String temp = fileName;
                        temp = temp.replaceAll("_\\d+(?=\\.)", "");
                        temp = temp.replaceAll("resp_", "").substring(2);
                        temp = temp.replaceAll("rise_", "");
                        Pattern value = Pattern.compile("\\d+");
                        Matcher data = value.matcher(temp);
                        data.find();
                        batchId = data.group();
                        batchIds.add(Integer.parseInt(batchId));
                        vo.setBatchIds(batchIds);
                        List<String> errorClaimNumbers = readResponse(vo, file, userActionCode);
                        claimInvoiceIntegrationDao.updateClaimBatch(batchId);
                        claimInvoiceIntegrationDao.updateFeedStatusToSuccess(batchId, userActionCode, errorClaimNumbers);
                        sapClaimInvoiceResponseMail(vo);
                    }
                }
            }
        }
		return null;
	}

	private void sapClaimInvoiceResponseMail(IntegrationReportVO integrationReportVO) {
		logger.info("Request for sap claim invoice feed ResponseMail");
		Set<Integer> batchIds = integrationReportVO.getBatchIds();
		if (batchIds != null && !batchIds.isEmpty()) {
			for (Integer batchId : batchIds) {
				List<SapClaimFeed> sapClaimFeeds = claimInvoiceIntegrationDao.getClaimInvoiceBatchByBatchId(batchId);
				List<Integer> sapClaimCount = claimInvoiceIntegrationDao.getSapClaimCount(batchId);
				integrationReportVO.setEmailContent(setFileEmailBody(batchId, sapClaimFeeds,
						integrationReportVO.getEmailContent(), integrationReportVO.getIsResponseMail(), sapClaimCount));
				sendResponceMail(integrationReportVO);
			}
		}
	}

	private void sendResponceMail(IntegrationReportVO vo) {
		sendSuccesReportMail(vo);
		sendErrorReportMail(vo, Constants.SAP_CLAIM_FEED_INTERFACE_RES_ERR_NOTIFICATION_CODE);
	}

	private void sendErrorReportMail(IntegrationReportVO vo, Integer notificationTypeId) {
		if (vo.getEmailContent().getError().length() != 0) {
			Set<NotificationRecipient> dynamicRecipients = fetchDynamicEmailRecipients();
			sendSapClaimFeedNotification(Boolean.FALSE, vo, notificationTypeId, dynamicRecipients);
			vo.setEmailContent(vo.new EmailContent());
		}
	}

    public Set<NotificationRecipient> fetchDynamicEmailRecipients() {
        Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
        String emailAddress = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_EXTERNAL_USER_FEED_EMAIL_RECIPIENT);
        if (emailAddress != null && !emailAddress.isEmpty()) {
            String[] singleEmailAddress = emailAddress.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)");
            if (singleEmailAddress.length > 0) {
                for (String recipeientMailAddress : singleEmailAddress) {
                    commonService.setNotificationRecipientsforNonEmployees(recipeientMailAddress, Constants.NOTIFICATION_RECIPIENT_TYPE_TO,
                            dynamicEmailRecipients);
                }
            }
        }
        return dynamicEmailRecipients;
    }

	private void sendSuccesReportMail(IntegrationReportVO vo) {
		if (vo.getEmailContent().getSuccess().length() != 0) {
			Set<NotificationRecipient> dynamicRecipients = fetchDynamicEmailRecipients();
			sendSapClaimFeedNotification(Boolean.TRUE, vo, Constants.SAP_CLAIM_FEED_INTERFACE_RES_NOTIFICATION_CODE, dynamicRecipients);
            if (vo.getEmailContent().getError().length() == 0) {
                vo.setEmailContent(vo.new EmailContent());
            }
        }
	}

	private List<String> readResponse(IntegrationReportVO vo, File file, String userActionCode) {
		BufferedReader fileReader = null;
        Boolean errorInProcess = Boolean.FALSE;
        List<String> errorClaimNumbers = new ArrayList<>();
        try {
            final int BATCH_ID = 0;
            final int CLAIM_NUMBER = 1;
            final int INVOICE_ID = 2;
            final int DOCUMENT_NUMBER = 3;
            final int FISCAL_YEAR = 4;
            final int STATUS = 5;
            final int MESSAGE = 6;
            List<ClaimInvoiceLog> claimInvoiceLogs = new ArrayList<>();
            fileReader = new BufferedReader(new FileReader(file.getAbsoluteFile()));
            fileReader.readLine();
            String line = "";           
            while ((line = fileReader.readLine()) != null) {
                String[] tokens = line.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)");
                if (tokens.length > 0) {
                    while (tokens.length < 5) {
                        String tempLine = fileReader.readLine();
                        if (tempLine == null) {
                            break;
                        }
                        line = line + tempLine;
                        tokens = line.split("\",\"", -1);
                    }
                    ClaimInvoiceLog claimInvoiceLog = new ClaimInvoiceLog(
                            (int)Double.parseDouble(tokens[INVOICE_ID].replace("\"", "")), tokens[CLAIM_NUMBER].replace("\"", ""),
                            tokens[FISCAL_YEAR] != null || !tokens[FISCAL_YEAR].isEmpty() ?  tokens[FISCAL_YEAR].replace("\"", "") : null, (int)Double.parseDouble(tokens[BATCH_ID].replace("\"", "")), 
                            tokens[DOCUMENT_NUMBER] != null || !tokens[DOCUMENT_NUMBER].isEmpty() ? tokens[DOCUMENT_NUMBER].replace("\"", "") : null, tokens[STATUS].replace("\"", ""), tokens[MESSAGE].replace("\"", ""));
                    claimInvoiceLogs.add(claimInvoiceLog);
                }
            }
            Map<Integer, List<ClaimInvoiceLog>> groupedClaimInvoiceLog =  claimInvoiceLogs
    				.stream().collect(Collectors.groupingBy(ClaimInvoiceLog :: getClaimInvoiceLogId));
            List<SapClaimFeedResponseMessage> responseMsgs = new ArrayList<>();
            groupedClaimInvoiceLog.values().stream().forEach(log -> {
            	log.forEach(msg -> {
            		SapClaimFeedResponseMessage responseMsg = new SapClaimFeedResponseMessage();
                	responseMsg.setMessage(msg.getMessage());
                	responseMsg.setClaimInvoiceLogId(msg.getClaimInvoiceLogId());
                	responseMsg.setUpdateUser("quickstart");
                	responseMsgs.add(responseMsg);
            	});
            	if (log != null && !log.isEmpty() && log.get(0) != null && log.get(0).getStatus().equals("S")) {
            		ClaimInvoiceLog claimInvoiceLog = claimInvoiceIntegrationDao.getClaimInvoiceLogById(log.get(0).getClaimInvoiceLogId());
            	}
            });
            claimInvoiceLogs.forEach(invoiceLog -> {
            	claimInvoiceIntegrationDao.saveOrUpdateClaimInvoiceLog(invoiceLog);
            	claimInvoiceIntegrationDao.updateClaimStatus(invoiceLog.getClaimNumber(),invoiceLog.getStatus());
            	if(invoiceLog.getStatus().equals("E")) {          		
            		claimInvoiceIntegrationDao.updateClaimSapFeedStatus(invoiceLog.getClaimNumber(), invoiceLog.getBatchId(), userActionCode);   
            		errorClaimNumbers.add(invoiceLog.getClaimNumber());
            	}
            });
            responseMsgs.forEach(mesg -> {
            	claimInvoiceIntegrationDao.saveOrUpdateClaimSapResponseMsg(mesg);
            });
            fileReader.close();
        } catch (Exception e) {
            logger.error("Error in method claim readResponse", e);
            e.printStackTrace();
            errorInProcess = Boolean.TRUE;
			vo.getEmailContent().getError().append("<br/>").append("Error in readResponce").append(e).append("<br/>");
        } finally {
            try {
                fileReader.close();
            } catch (IOException e) {
                e.printStackTrace();
                errorInProcess = Boolean.TRUE;
            }
        }
        if(errorInProcess.equals(Boolean.FALSE))
        	moveFile(file, null);
        return errorClaimNumbers;      
	}
	
	private void sentEInvoiceNotification(ClaimInvoiceLog claimInvoiceLog) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(Constants.SAP_CLAIM_FEED_E_INVOICE_NOTIFICATION_CODE);
		emailServiceVO.setModuleCode(Constants.CLAIM_MODULE_CODE);
		emailServiceVO.setModuleItemKey(claimInvoiceLog.getClaimId().toString());
		emailServiceVO.setPlaceHolder(getEInvoicePlaceholders(claimInvoiceLog));
		emailServiceVO.setSubModuleCode(Constants.SUBMODULE_ITEM_KEY);
		emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		emailService.sendEmail(emailServiceVO);
	}

	private Map<String, String> getEInvoicePlaceholders(ClaimInvoiceLog claimInvoiceLog) {
		Map<String, String> placeHolder = new HashMap<>();
		placeHolder.put("{OUTPUT_DOCUMENT_NUMBER}", claimInvoiceLog.getOutputDocumentNumber() == null  ? "" : claimInvoiceLog.getOutputDocumentNumber());
		return placeHolder;
	}

	private void moveFile(File file, EmailContent emailBody) {
        try {
        	String outbound = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.RISE_CLAIM_INVOICE_OUTBOUND_ARCHIVE);
            Path temp = Files.move(Paths.get(file.getPath()), Paths.get(outbound + "/" + file.getName()),
            		StandardCopyOption.REPLACE_EXISTING);
			if (temp != null) {
			    logger.info("file successfully moved from {} -> {} ", file.getPath(), temp);
			    if (commonDao.getParameterValueAsBoolean(Constants.IS_SFTP_ENABLED)) {
			       moveSftpDirectorys(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_CLAIM_INVOICE_OUTBOUND), file.getName(), sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_CLAIM_INVOICE_OUTBOUND_ARCHIVE), null);
			    }
			} else {
			    logger.info("File not moved : {} ", file.getPath());
			}
		} catch (Exception e) {
		    logger.error("Exception in method moveFile : {} ", e);
		    e.printStackTrace();
		}
	}
	
	public void moveSftpDirectorys(String sourceDir, String fileName, String destination, Integer fileId) throws Exception {
        String sftpHost = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_HOST);
        int sftpPort = Integer.parseInt(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_PORT));
        String sftpUser = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_USER);
        com.jcraft.jsch.Session session = null;
        Channel channel = null;
        ChannelSftp channelSftp = null;
        logger.info("preparing the host information for sftp.");
        try {
            JSch jsch = new JSch();
            session = jsch.getSession(sftpUser, sftpHost, sftpPort);
            session.setPassword(fastSftpPassword);
            java.util.Properties config = new java.util.Properties();
            config.put("StrictHostKeyChecking", "no");
            session.setConfig(config);
            session.connect();
            logger.info("Host connected.");
            channel = session.openChannel("sftp");
            channel.connect();
            logger.info("sftp channel opened and connected.");
            channelSftp = (ChannelSftp) channel;
            channelSftp.cd(sourceDir);
            List<String> list = new ArrayList<>();
            @SuppressWarnings("unchecked")
			Vector<LsEntry> files = channelSftp.ls(sourceDir);
            for (LsEntry entry : files) {
                if (!entry.getFilename().equals(".") && !entry.getFilename().equals("..") && fileName.equals(entry.getFilename())) {
                    list.add(entry.getFilename());
                    String sourceFile = new StringBuilder(sourceDir).append(entry.getFilename()).toString();
                    String destinationFile = new StringBuilder(destination).append(entry.getFilename()).toString();
                    channelSftp.rename(sourceFile, destinationFile);
                    channelSftp.cd(destination);
                    if (checkFileInArchive(channelSftp, fileName)) {                       
                        logger.info("file successfully moved from {} -> {} ", sourceFile, destinationFile);
                    }
                }
            }
        } catch (Exception ex) {
            logger.info("Exception found while tranfer the response.{}", ex);
            ex.printStackTrace();
            throw ex;
        } finally {
            channelSftp.exit();
            logger.info("sftp Channel exited.");
            channel.disconnect();
            logger.info("Channel disconnected.");
            session.disconnect();
            logger.info("Host Session disconnected.");
        }
    }
	
	private boolean checkFileInArchive(ChannelSftp channelSftp, String fileName) {
        try {
            channelSftp.lstat(fileName);
            return true;
        } catch (SftpException e) {
            logger.error("Error occured in checkFileInArchive due to : {}", e.getMessage());
            e.printStackTrace();
            return false;
        }
	}
}
