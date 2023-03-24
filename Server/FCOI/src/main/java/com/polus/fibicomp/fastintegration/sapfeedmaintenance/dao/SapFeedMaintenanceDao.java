package com.polus.fibicomp.fastintegration.sapfeedmaintenance.dao;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;
import com.polus.fibicomp.fastintegration.pojo.SapAwardFeed;
import com.polus.fibicomp.fastintegration.pojo.SapFeedStatus;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplFmBudget;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplFundedPrgm;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplGrantBudMaster;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplGrantMaster;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplProjectDef;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplSponsoPrgm;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplSponsorClass;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplWbs;
import com.polus.fibicomp.fastintegration.pojo.SapFeedType;
import com.polus.fibicomp.fastintegration.pojo.SapFeedUserAction;
import com.polus.fibicomp.fastintegration.sapfeedmaintenance.vo.SapFeedMaintenanceVO;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestType;

@Transactional
@Service
public interface SapFeedMaintenanceDao {

	/**
	 * This method is used to get sap award feed batch detail
	 * @param vo
	 * @return list of sap award feed.
	 */
	public List<SapAwardFeed> getSapFeedMaintenanceBatchDetail(SapFeedMaintenanceVO vo);

	/**
	 * This method is used to get sap award feed status
	 * @return list of sap award feed status.
	 */
	public List<SapFeedStatus> getSapAwardFeedStatus();

	/**
	 * This method is used to get latest batch id
	 * @return batch id
	 */
	public Integer getLatesSapAwardFeedBatchId();

	/**
	 * This method is used to get sap feed status by code
	 * @param feedStatusCode
	 * @return SapFeedStatus
	 */
	public SapFeedStatus getSapFeedStatusByCode(String feedStatusCode);

	/**
	 * This method is used to get sap feed type by code
	 * @param feedTypeCode
	 * @return SapFeedType
	 */
	public SapFeedType getSapFeedTypeByCode(String feedTypeCode);

	/**
	 * This method is used to get SapFeedTmplFmBudget by feedId
	 * @param feedId
	 * @return list of SapFeedTmplFmBudget
	 */
	public List<SapFeedTmplFmBudget> getSapFeedTmplFmBudgetbyFeedId(Integer feedId);

	/**
	 * This method is used to get SapFeedTmplFundedPrgm by feedId
	 * @param feedId
	 * @return list of SapFeedTmplFundedPrgm
	 */
	public List<SapFeedTmplFundedPrgm> getSapFeedTmplFundedPrgmByFeedId(Integer feedId);

	/**
	 * This method is used to get SapFeedTmplGrantBudMaster by feedId
	 * @param feedId
	 * @return list of SapFeedTmplGrantBudMaster
	 */
	public List<SapFeedTmplGrantBudMaster> getSapFeedTmplGrantBudMasterByFeedId(Integer feedId);

	/**
	 * This method is used to get SapFeedTmplGrantMaster by feedId
	 * @param feedId
	 * @return list of SapFeedTmplGrantMaster
	 */
	public List<SapFeedTmplGrantMaster> getSapFeedTmplGrantMasterByFeedId(Integer feedId);

	/**
	 * This method is used to get SapFeedTmplProjectDef by feedId
	 * @param feedId
	 * @return list of SapFeedTmplProjectDef
	 */
	public List<SapFeedTmplProjectDef> getSapFeedTmplProjectDefByFeedId(Integer feedId);

	/**
	 * This method is used to get SapFeedTmplSponsoPrgm by feedId
	 * @param feedId
	 * @return list of SapFeedTmplSponsoPrgm
	 */
	public List<SapFeedTmplSponsoPrgm> getSapFeedTmplSponsoPrgmByFeedId(Integer feedId);

	/**
	 * This method is used to get SapFeedTmplSponsorClass by feedId
	 * @param feedId
	 * @return list of SapFeedTmplSponsorClass
	 */
	public List<SapFeedTmplSponsorClass> getSapFeedTmplSponsorClasseByFeedId(Integer feedId);

	/**
	 * This method is used to get SapFeedTmplWbs by feedId
	 * @param feedId
	 * @return list of SapFeedTmplWbs
	 */
	public List<SapFeedTmplWbs> getSapFeedTmplWbsByFeedId(Integer feedId);

	/**
	 * This method is used to get SapAwardFeed type by feedId
	 * @param feedId
	 * @return list of SapAwardFeed
	 */
	public List<SapAwardFeed> getBatchHistoryDetail(SapFeedMaintenanceVO vo);

	/**
	 * This method is used to update sap award feed by params
	 * @param feedIds
	 * @param userComment
	 * @param userAction
	 */
	public void updateSapAwardFeedById(String userComment, String userActionCode, List<Integer> feedIds);

	/**
	 * This method is used to get sap award feed by feed ids
	 * @param feedIds
	 * @return list of SapAwardFeed
	 */
	public List<SapAwardFeed> getSapAwardFeeds(List<Integer> feedIds);

	/**
	 * This method is used to update sap award feed by params
	 * @param feedIds
	 * @param userComment
	 * @param userAction
	 * @param status
	 * @param changeStatus
	 */
	public void updateSapAwardFeedByStatus(String status, String changeStatus, String userComment, String userActionCode,  List<Integer> feedIds);

	/**
	 * This method is used to update sap award feed by params
	 * @param feedId
	 * @param userAction
	 * @param userComment 
	 */
	public void updateSapAwardFeedUserAction(String userAction, String userComment, Integer feedId);

	/**
	 * This method is used to re interface sap award feed by params
	 * @param feedId
	 * @param awardId
	 * @param awardNumber
	 * @param sequenceNumber
	 * @param updateUser
	 * @param feedType
	 * @return list of feed Id
	 */
	public Integer reInterfaceSapAwardFeed(Integer awardId, String awardNumber, Integer sequenceNumber, String updateUser, String feedType);

	/**
	 * This method is used to update sponsor class
	 * @param feedIds
	 * @param userComment 
	 */
	public void updateSponsorClass(List<Integer> feedIds, String userComment);

	/**
	 * This method is used to update sponsor program
	 * @param feedIds
	 * @param userComment 
	 */
	public void updateSponsoPrgm(List<Integer> feedIds, String userComment);

	/**
	 * This method is used to update funded program
	 * @param feedIds
	 * @param userComment 
	 */
	public void updateFundedPrgm(List<Integer> feedIds, String userComment);

	/**
	 * This method is used to update grant master
	 * @param feedIds
	 * @param userComment 
	 */
	public void updateGrantMaster(List<Integer> feedIds, String userComment);

	/**
	 * This method is used to update project definition
	 * @param feedIds
	 * @param userComment 
	 */
	public void updateProjectDef(List<Integer> feedIds, String userComment);

	/**
	 * This method is used to update grant budget master 
	 * @param feedIds
	 * @param userComment 
	 */
	public void updateGrantBudMaster(List<Integer> feedIds, String userComment);

	/**
	 * This method is used to update WBS
	 * @param feedIds
	 * @param userComment 
	 */
	public void updateWBS(List<Integer> feedIds, String userComment);

	/**
	 * This method is used to update Fm Budget
	 * @param feedIds
	 * @param userComment 
	 */
	public void updateFmBudgets(List<Integer> feedIds, String userComment);

	/**
	 * This method is used to get award details by params
	 * @param awardNumber
	 * @param sequenceNumber
	 * @return 
	 */
	public AwardBudgetHeader getAwardBudgetHeaderByParam(String awardNumber, Integer sequenceNumber);

	/**
	 * This method is used to get service request type
	 * @param awardId
	 * @return 
	 */
	public ServiceRequestType getServiceRequestTypeByAwardId(Integer awardId);

	/**
	 * This method is used to get sap award feed actions
	 * @return list of SapFeedUserAction
	 */
	public List<SapFeedUserAction> getSapFeedUserAction();

	/**
	 * This method is used to get sap award feed actions by user action code
	 * @param userActionCode
	 * @return object of SapFeedUserAction
	 */
	public SapFeedUserAction getSapFeedUserActionByCode(String userActionCode);

	/**
	 * @param feedId
	 * @return
	 */
	public List<SapAwardFeed> getAllConcurrentFeedIds(Integer feedId, List<String> feedStatuses);

	/**
	 * @param vo
	 * @return
	 */
	public Integer getSapFeedMaintenanceBatchDetailCount(SapFeedMaintenanceVO vo);

	/**
	 * @param vo
	 * @return
	 */
	public Integer getBatchHistoryDetailCount(SapFeedMaintenanceVO vo);

}
