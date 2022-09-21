package com.polus.fibicomp.award.datesandamounts.service;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ReflectionUtils;

import com.polus.fibicomp.award.comparator.DatesAndAmountComparator;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.datesandamounts.dao.DatesAndAmountDao;
import com.polus.fibicomp.award.datesandamounts.dto.AwardAmountFNADistributionDTO;
import com.polus.fibicomp.award.datesandamounts.dto.AwardFunds;
import com.polus.fibicomp.award.datesandamounts.dto.AwardFundsDTO;
import com.polus.fibicomp.award.datesandamounts.dto.DatesAndAmountsDTO;
import com.polus.fibicomp.award.datesandamounts.pojo.AwardAmountFNADistribution;
import com.polus.fibicomp.award.datesandamounts.pojo.AwardAmountTransactionHistory;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardAmountInfo;
import com.polus.fibicomp.award.pojo.AwardAmountTransaction;
import com.polus.fibicomp.award.pojo.AwardCostShare;
import com.polus.fibicomp.award.service.AwardService;
import com.polus.fibicomp.award.vo.AwardDatesandAmountVO;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.budget.dao.AwardBudgetDao;
import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;
import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.ip.pojo.InstituteProposal;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.security.AuthenticatedUser;
@Transactional
@Service(value = "datesAndAmountService")
public class DatesAndAmountServiceImpl implements DatesAndAmountService{

	protected static Logger logger = LogManager.getLogger(DatesAndAmountServiceImpl.class.getName());

	@Autowired
	private DatesAndAmountDao datesAndAmountDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private AwardService awardService;

	@Autowired
	private AwardBudgetDao awardBudgetDao;

	private static final String VALIDATION_MESSAGE = "Invalid Transaction – Insufficient funds in Award #";

	@Override
	public String getAwardDatesAndAmount(AwardDatesandAmountVO awardDatesandAmountVO) throws Exception {
		awardDatesandAmountVO.setAwardTransactionTypes(datesAndAmountDao.getAllAwardTransactionTypes());
		if (awardDatesandAmountVO.getAwardNumber() != null) {
			List<String> source = new ArrayList<>();
			source.add(Constants.EXTERNAL);
			source.add(Constants.INTERNAL);
			source.addAll(datesAndAmountDao.getAwardNumbersInHierarchy(awardDatesandAmountVO.getAwardNumber()));
			awardDatesandAmountVO.setSourceAccount(source);
			awardDatesandAmountVO.setDestinationAccount(datesAndAmountDao.getAwardNumbersInHierarchy(awardDatesandAmountVO.getAwardNumber()));
		}
		awardDatesandAmountVO.setAwardAmountInfos(prepareAwardAmountInfoForAward(awardDatesandAmountVO.getAwardId(), awardDatesandAmountVO.getAwardNumber(), awardDatesandAmountVO.getAwardSequenceNumber()));
		awardDatesandAmountVO.setIsDatesAndAmountsEditable(isDatesAndAmountEditable(awardDatesandAmountVO.getAwardNumber()));
		return commonDao.convertObjectToJSON(awardDatesandAmountVO);
	}

	private Boolean isDatesAndAmountEditable(String awardNumber) {
		AwardAmountTransaction awardAmountTransaction = datesAndAmountDao.getIsDatesAndAmountsEditable((awardNumber).split("-")[0]);
		return (awardAmountTransaction == null || (awardAmountTransaction !=null && awardAmountTransaction.getAwardNumber().equals(awardNumber)));
	}

	@Override
	public List<AwardAmountInfo> prepareAwardAmountInfoForAward(Integer awardId, String awardNumber, Integer awardSequenceNumber) {
		String awardSequenceStatus = awardDao.getAwardSequenceStatusByAwardId(awardId);
		List<AwardAmountInfo> awardAmountInfos = getAwardAmountInfoBasedOnParams(awardId, awardNumber, awardSequenceNumber, awardSequenceStatus);
		if (awardSequenceStatus.equals(Constants.AWARD_FINAL_STATUS_PENDING)) {
			getAllUnrelatedTransactions(awardNumber, awardAmountInfos);
		}
		if (!awardAmountInfos.isEmpty()) {
			Collections.sort(awardAmountInfos, new DatesAndAmountComparator());
			awardAmountInfos.stream().forEach(awardAmountInfo -> {
				if (awardAmountInfo.getAwardAmountTransaction() != null && awardAmountInfo.getAwardAmountTransaction().getFundedProposalId() != null) {
					String proposalNumber = datesAndAmountDao.getProposalNumberBasedOnProposalId(awardAmountInfo.getAwardAmountTransaction().getFundedProposalId());
					awardAmountInfo.getAwardAmountTransaction().setFundingProposalNumber(proposalNumber);
				}
			});
			getFullNameOfUpdateUser(awardAmountInfos);
		}
		return awardAmountInfos;
	}

	private List<AwardAmountInfo> getAllUnrelatedTransactions(String awardNumber, List<AwardAmountInfo> awardAmountInfos) {
		List<BigDecimal> transactionIds = new ArrayList<>();
		awardAmountInfos.forEach(awardAmountInfo -> {
			transactionIds.add(awardAmountInfo.getTransactionId());
		});
		List<AwardAmountInfo> unrelatedTransaction = datesAndAmountDao.getAllUnrelatedTransactions(transactionIds, awardNumber); 
		awardAmountInfos.addAll(unrelatedTransaction);
		return awardAmountInfos;
	}

	private void getFullNameOfUpdateUser(List<AwardAmountInfo> awardAmountInfos) {
		Set<String> userName = awardAmountInfos.stream().map(AwardAmountInfo::getUpdateUser).collect(Collectors.toSet());
		if(!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream().collect(Collectors
					.toMap(person -> person.getPrincipalName().toUpperCase(), person -> person.getFullName()));
			awardAmountInfos.stream().filter(item -> item.getUpdateUser() != null)
					.filter(item -> collect.containsKey(item.getUpdateUser().toUpperCase()))
					.forEach(item -> item.setUpdateUserFullName(collect.get(item.getUpdateUser().toUpperCase())));
		}
	}

	@Override
	public List<AwardAmountInfo> getAwardAmountInfoBasedOnParams(Integer awardId, String awardNumber, Integer awardSequenceNumber, String awardSequenceStatus) {
		BigDecimal transactionId  = datesAndAmountDao.fetchLatestTransactionCode(awardId);
		return datesAndAmountDao.getAwardAmountInfoBasedOnParams(awardNumber, transactionId, awardSequenceNumber, awardSequenceStatus);
	}

	@Override
	public String saveTransactionDetails(AwardDatesandAmountVO awardDatesandAmountVO) throws Exception {
		try {
			String currentAwardNumber = awardDatesandAmountVO.getAwardAmountInfo().getAwardNumber();
			if (awardDatesandAmountVO.getAwardAmountInfo().getAwardAmountInfoId() != null && awardDatesandAmountVO.getAwardAmountInfo().getAwardAmountTransaction() != null &&  awardDatesandAmountVO.getAwardAmountInfo().getAwardAmountTransaction().getAwardNumber() != null) {
				currentAwardNumber = awardDatesandAmountVO.getAwardAmountInfo().getAwardAmountTransaction().getAwardNumber();
			}
			if (Boolean.TRUE.equals((isDatesAndAmountEditable(currentAwardNumber)))) {
				AwardAmountInfo awardAmountInfo = awardDatesandAmountVO.getAwardAmountInfo();
				AwardAmountInfo deletedAwardAmountInfo = new AwardAmountInfo();
				Boolean isUpdate = false;
				if (awardAmountInfo.getAwardAmountInfoId() != null) {
					isUpdate = true;
					deletedAwardAmountInfo = datesAndAmountDao.getAwardAmountInfoBasedOnAwardDetail(awardAmountInfo.getAwardAmountInfoId());
					awardAmountInfo.setAwardAmountInfoId(null);
					deleteTransactionDetails(awardAmountInfo.getTransactionId());
				}
				DatesAndAmountsDTO datesAndAmountsDTO = new DatesAndAmountsDTO();
				Integer currentAwardId = awardDatesandAmountVO.getAwardAmountInfo().getAwardId();
				Integer currentAwardSequenceNumber = awardDao.getAwardSequenceNumberBasedOnAwardId(awardDatesandAmountVO.getAwardAmountInfo().getAwardId());
				String sourceAwardNumber = awardAmountInfo.getAwardAmountTransaction().getSourceAwardNumber();
				String destinationAwardNumber = awardAmountInfo.getAwardAmountTransaction().getDestinationAwardNumber();
				List<String> sourceParentAwardNumbers = new ArrayList<>();
				List<String> destinationParentAwardNumbers = new ArrayList<>();
				if (sourceAwardNumber == null && destinationAwardNumber == null) {
					sourceAwardNumber = Constants.EXTERNAL;
					destinationAwardNumber = awardAmountInfo.getAwardNumber();
					awardAmountInfo.getAwardAmountTransaction().setSourceAwardNumber(sourceAwardNumber);
					awardAmountInfo.getAwardAmountTransaction().setDestinationAwardNumber(destinationAwardNumber);
				}
				AwardAmountTransaction awardAmountTransaction = prepareAwardAmountTransaction(awardDatesandAmountVO.getAwardAmountInfo());
				List<String> awardNumbers = new ArrayList<>();
				if ((sourceAwardNumber.equals(Constants.EXTERNAL) || sourceAwardNumber.equals(Constants.INTERNAL)) && destinationAwardNumber != null) {
					awardNumbers = datesAndAmountDao.getChildAwardNumbersBasedOnParentAwardNumber(destinationAwardNumber);
				}
				if ((!sourceAwardNumber.equals(Constants.EXTERNAL) && !sourceAwardNumber.equals(Constants.INTERNAL)) && destinationAwardNumber != null) {
					if ((awardAmountInfo.getObligatedChange().compareTo(BigDecimal.ZERO) < 0) || (awardAmountInfo.getAnticipatedChange().compareTo(BigDecimal.ZERO) < 0)) {
						AwardAmountInfo destAwardAmountInfoData = datesAndAmountDao.getLatestAwardAmountInfo(destinationAwardNumber);
						if (destAwardAmountInfoData == null || (destAwardAmountInfoData != null && (awardAmountInfo.getObligatedChange() .add(destAwardAmountInfoData.getObliDistributableAmount())
										.compareTo(BigDecimal.ZERO) < 0) && (awardAmountInfo.getAnticipatedChange().add(destAwardAmountInfoData.getAntDistributableAmount())
										.compareTo(BigDecimal.ZERO) < 0))) {
							awardDatesandAmountVO.setMessage(VALIDATION_MESSAGE + destinationAwardNumber);
							datesAndAmountDao.deleteAwardAmountTransaction(awardAmountTransaction);
							if (Boolean.TRUE.equals(isUpdate)) {
								restoreAwardAmountInfo(awardDatesandAmountVO, deletedAwardAmountInfo);
							}
							return commonDao.convertObjectToJSON(awardDatesandAmountVO);
						}
						AwardBudgetHeader awardBudgetHeader = awardBudgetDao.getLatestAwardBudgetHeader(destinationAwardNumber);
						if (awardBudgetHeader != null  && awardBudgetHeader.getAvailableFundType() != null &&
								awardBudgetHeader.getAvailableFundType().equals(Constants.OBLIGATED_DISTRIBUTABLE) && awardAmountInfo.getObligatedChange().compareTo(BigDecimal.ZERO) < 0 && 
								destAwardAmountInfoData.getObliDistributableAmount().subtract(awardBudgetHeader.getTotalCost()).add(awardAmountInfo.getObligatedChange()).compareTo(BigDecimal.ZERO) < 0) {
							awardDatesandAmountVO.setMessage(VALIDATION_MESSAGE + destinationAwardNumber);
							datesAndAmountDao.deleteAwardAmountTransaction(awardAmountTransaction);
							if (Boolean.TRUE.equals(isUpdate)) {
								restoreAwardAmountInfo(awardDatesandAmountVO, deletedAwardAmountInfo);
							}
							return commonDao.convertObjectToJSON(awardDatesandAmountVO);
						}
					}
					AwardAmountInfo awardAmountInfoData = datesAndAmountDao.getLatestAwardAmountInfo(sourceAwardNumber);
					if (awardAmountInfoData != null && (awardAmountInfo.getObligatedChange()
							.compareTo(awardAmountInfoData.getObliDistributableAmount()) <= 0
							&& (awardAmountInfo.getAnticipatedChange().compareTo(awardAmountInfoData.getAntDistributableAmount()) <= 0))) {
						AwardBudgetHeader awardBudgetHeader = awardBudgetDao.getLatestAwardBudgetHeader(sourceAwardNumber);
						if (awardBudgetHeader != null && awardBudgetHeader.getAvailableFundType() != null &&
								awardBudgetHeader.getAvailableFundType().equals(Constants.OBLIGATED_DISTRIBUTABLE) && awardAmountInfoData.getObliDistributableAmount().subtract(awardBudgetHeader.getTotalCost())
								.subtract(awardAmountInfo.getObligatedChange()).compareTo(BigDecimal.ZERO) < 0) {
							awardDatesandAmountVO.setMessage(VALIDATION_MESSAGE + sourceAwardNumber);
							datesAndAmountDao.deleteAwardAmountTransaction(awardAmountTransaction);
							if (Boolean.TRUE.equals(isUpdate)) {
								restoreAwardAmountInfo(awardDatesandAmountVO, deletedAwardAmountInfo);
							}
							return commonDao.convertObjectToJSON(awardDatesandAmountVO);
						}
						datesAndAmountsDTO = datesAndAmountDao.getAllIntermediateChildAwards(sourceAwardNumber, destinationAwardNumber);
						awardNumbers = datesAndAmountsDTO.getAwardNumbers();
						awardDatesandAmountVO.setCanAddTotal(datesAndAmountsDTO.isCanAddTotal());
						awardDatesandAmountVO.setUnRelatedTransaction(datesAndAmountsDTO.isUnRelatedTransaction());
					} else {
						awardDatesandAmountVO.setMessage(VALIDATION_MESSAGE + sourceAwardNumber);
						datesAndAmountDao.deleteAwardAmountTransaction(awardAmountTransaction);
						if (Boolean.TRUE.equals(isUpdate)) {
							restoreAwardAmountInfo(awardDatesandAmountVO, deletedAwardAmountInfo);
						}
						return commonDao.convertObjectToJSON(awardDatesandAmountVO);
					}
				}
				if (awardDatesandAmountVO.isUnRelatedTransaction()) {
					awardNumbers.clear();
					sourceParentAwardNumbers = datesAndAmountDao.getChildAwardNumbersBasedOnParentAwardNumber(sourceAwardNumber);
					destinationParentAwardNumbers = datesAndAmountDao.getChildAwardNumbersBasedOnParentAwardNumber(destinationAwardNumber);
					List<String> allParentAwardNumbers = new ArrayList<>();
					allParentAwardNumbers.addAll(sourceParentAwardNumbers);
					allParentAwardNumbers.addAll(destinationParentAwardNumbers);
					if (allParentAwardNumbers != null) {
						Set<String> allParentAwardNumbersSet = new HashSet<>(allParentAwardNumbers);
						awardNumbers = allParentAwardNumbersSet.stream().collect(Collectors.toList());
					}
				}
				if (sourceAwardNumber != null && (sourceAwardNumber.equals(Constants.EXTERNAL)
						|| sourceAwardNumber.equals(Constants.INTERNAL)) && destinationAwardNumber != null) {
					for (String awardNumber : awardNumbers) {
						Award award = null;
						if (awardNumber.equals(currentAwardNumber)) {
							award = new Award();
							award.setAwardId(currentAwardId);
							award.setAwardNumber(currentAwardNumber);
							award.setSequenceNumber(currentAwardSequenceNumber);
						} else {
							award = setAwardDetails(award, awardNumber);
						}
						AwardDatesandAmountVO vo = prepareAwardAmountInfoDataForSave(awardDatesandAmountVO,
								awardAmountTransaction);
						if (awardNumber.equals(destinationAwardNumber)) {
							vo.setDestinationAccount(true);
						} else {
							vo.setExternalTransaction(true);
						}
						vo.getAwardAmountInfo().getAwardAmountTransaction().setAwardNumber(currentAwardNumber);
						if (award != null) {
							vo = prepareAwardAmountInfoObject(vo, award);
							saveAwardAmountInfo(vo);
						}
					}
				} else if (sourceAwardNumber != null && destinationAwardNumber != null) {
					awardNumbers.remove(sourceAwardNumber);
					if (sourceAwardNumber.equals(destinationAwardNumber)) {
						awardDatesandAmountVO.setDestinationAccount(true);
					}
					boolean canCreateSubTransactions = false;
					Award sourceActiveAward = null;
					if (sourceAwardNumber.equals(currentAwardNumber)) {
						sourceActiveAward = new Award();
						sourceActiveAward.setAwardId(currentAwardId);
						sourceActiveAward.setAwardNumber(currentAwardNumber);
						sourceActiveAward.setSequenceNumber(currentAwardSequenceNumber);
					} else {
						sourceActiveAward = setAwardDetails(sourceActiveAward, sourceAwardNumber);
					}
					if (sourceActiveAward != null) {
						awardDatesandAmountVO.setAwardNumberFoundInSource(true);
						AwardAmountInfo awardAmountInfoData = datesAndAmountDao.getLatestAwardAmountInfo(sourceActiveAward.getAwardNumber());
						if (awardAmountInfoData != null) {
							if (awardAmountInfo.getObligatedChange() .compareTo(awardAmountInfoData.getObliDistributableAmount()) <= 0
									&& (awardAmountInfo.getAnticipatedChange().compareTo(awardAmountInfoData.getAntDistributableAmount()) <= 0)) {
								canCreateSubTransactions = true;
								awardAmountInfo.setAwardId(sourceActiveAward.getAwardId());
								AwardDatesandAmountVO vo = prepareAwardAmountInfoDataForSave(awardDatesandAmountVO, awardAmountTransaction);
								vo.setSourceAccount(true);
								vo.getAwardAmountInfo().getAwardAmountTransaction().setAwardNumber(currentAwardNumber);
								vo.getAwardAmountInfo().setAwardNumber(sourceActiveAward.getAwardNumber());
								vo.getAwardAmountInfo().setSequenceNumber(sourceActiveAward.getSequenceNumber());
								saveAwardAmountInfo(vo);
							} else {
								awardDatesandAmountVO.setMessage("Invalid Transaction");
							}
						}
					}
					if (canCreateSubTransactions) {
						for (String awardNumber : awardNumbers) {
							if (awardDatesandAmountVO.isUnRelatedTransaction()) {
								awardDatesandAmountVO.setAwardNumberFoundInBoth(false);
								awardDatesandAmountVO.setAwardNumberFoundInSource(false);
								awardDatesandAmountVO.setAwardNumberFoundInDestination(false);
								if (sourceParentAwardNumbers.contains(awardNumber)
										&& destinationParentAwardNumbers.contains(awardNumber)) {
									awardDatesandAmountVO.setAwardNumberFoundInBoth(true);
								} else if (sourceParentAwardNumbers.contains(awardNumber)) {
									awardDatesandAmountVO.setAwardNumberFoundInSource(true);
								} else if (destinationParentAwardNumbers.contains(awardNumber)) {
									awardDatesandAmountVO.setAwardNumberFoundInDestination(true);
								}
							}
							Award award = null;
							if (awardNumber.equals(currentAwardNumber)) {
								award = new Award();
								award.setAwardId(currentAwardId);
								award.setAwardNumber(currentAwardNumber);
								award.setSequenceNumber(currentAwardSequenceNumber);
							} else {
								award = setAwardDetails(award, awardNumber);
							}
							if (award != null) {
								AwardDatesandAmountVO vo = prepareAwardAmountInfoDataForSave(awardDatesandAmountVO, awardAmountTransaction);
								if (awardNumber.equals(destinationAwardNumber)) {
									vo.setDestinationAccount(true);
								} else {
									vo.setInternalTransaction(true);
								}
								vo.getAwardAmountInfo().getAwardAmountTransaction().setAwardNumber(currentAwardNumber);
								vo = prepareAwardAmountInfoObject(vo, award);
								saveAwardAmountInfo(vo);
							}
						}
					}
				} else {
					awardDatesandAmountVO.setDestinationAccount(true);
					saveAwardAmountInfo(awardDatesandAmountVO);
				}
				return commonDao.convertObjectToJSON(awardDatesandAmountVO);
			} else {
				awardDatesandAmountVO.setMessage("Already a pending transaction is in progress in award hierarchy for another award");
				return commonDao.convertObjectToJSON(awardDatesandAmountVO);
			}
		} catch (Exception e) {
			return commonDao.convertObjectToJSON(awardDatesandAmountVO);
		}
	}

	private void restoreAwardAmountInfo(AwardDatesandAmountVO awardDatesandAmountVO, AwardAmountInfo awardAmountInfo) throws Exception {
		AwardAmountInfo deletedAwardAmountInfo = new AwardAmountInfo();
		ReflectionUtils.shallowCopyFieldState(awardAmountInfo, deletedAwardAmountInfo);
		deletedAwardAmountInfo.setAwardAmountInfoId(null);
		awardDatesandAmountVO.setAwardAmountInfo(deletedAwardAmountInfo);
		saveTransactionDetails(awardDatesandAmountVO);
	}

	private Award setAwardDetails(Award award, String awardNumber) {
		Award activeAward = awardDao.fetchActiveAwardByAwardNumber(awardNumber);
		if (activeAward == null) {
			activeAward = awardDao.fetchAwardSetUpByAwardNumber(awardNumber);
		}
		if (activeAward != null) {
			award = new Award();
			award.setAwardId(activeAward.getAwardId());
			award.setAwardNumber(activeAward.getAwardNumber());
			award.setSequenceNumber(activeAward.getSequenceNumber());
		}
		return award;
	}

	private AwardDatesandAmountVO prepareAwardAmountInfoObject(AwardDatesandAmountVO vo, Award award) {
		vo.getAwardAmountInfo().setAwardId(award.getAwardId());
		vo.getAwardAmountInfo().setAwardNumber(award.getAwardNumber());
		vo.getAwardAmountInfo().setSequenceNumber(award.getSequenceNumber());
		return vo;
	}

	private AwardDatesandAmountVO prepareAwardAmountInfoDataForSave(AwardDatesandAmountVO awardDatesandAmountVO, AwardAmountTransaction awardAmountTransaction) {
		AwardDatesandAmountVO vo = new AwardDatesandAmountVO();
		vo.setCanAddTotal(awardDatesandAmountVO.isCanAddTotal());
		vo.setUnRelatedTransaction(awardDatesandAmountVO.isUnRelatedTransaction());
		vo.setAwardNumberFoundInBoth(awardDatesandAmountVO.isAwardNumberFoundInBoth());
		vo.setAwardNumberFoundInSource(awardDatesandAmountVO.isAwardNumberFoundInSource());
		vo.setAwardNumberFoundInDestination(awardDatesandAmountVO.isAwardNumberFoundInDestination());
		AwardAmountInfo awardAmountInfoPassed = awardDatesandAmountVO.getAwardAmountInfo();
		AwardAmountInfo awardAmountInfo = new AwardAmountInfo();
		AwardAmountInfo latestAwardAmountInfo = datesAndAmountDao.getLatestAwardAmountInfo(awardAmountInfoPassed.getAwardNumber());
		awardAmountInfo.setAwardId(awardAmountInfoPassed.getAwardId());
		awardAmountInfo.setAnticipatedChange(awardAmountInfoPassed.getAnticipatedChange());
		if (awardAmountInfoPassed.getCurrentFundEffectiveDate() == null && latestAwardAmountInfo != null) {
			awardAmountInfoPassed.setCurrentFundEffectiveDate(latestAwardAmountInfo.getCurrentFundEffectiveDate());
		}
		if (awardAmountInfoPassed.getObligationExpirationDate() == null && latestAwardAmountInfo != null) {
			awardAmountInfoPassed.setObligationExpirationDate(latestAwardAmountInfo.getObligationExpirationDate());
		}
		awardAmountInfo.setCurrentFundEffectiveDate(awardAmountInfoPassed.getCurrentFundEffectiveDate());
		awardAmountInfo.setObligationExpirationDate(awardAmountInfoPassed.getObligationExpirationDate());
		awardAmountInfo.setFinalExpirationDate(awardAmountInfoPassed.getFinalExpirationDate());
		awardAmountInfo.setObligatedChange(awardAmountInfoPassed.getObligatedChange());
		awardAmountInfo.setUpdateUser(awardAmountInfoPassed.getUpdateUser());
		awardAmountInfo.setAwardAmountTransaction(awardAmountTransaction);
		awardAmountInfo.setTransactionId(awardAmountTransaction.getTransactionId());
		if (awardAmountInfoPassed.getUpdateTimestamp() != null) {
			awardAmountInfoPassed.setUpdateTimestamp(awardAmountInfoPassed.getUpdateTimestamp());
		}
		vo.setAwardAmountInfo(awardAmountInfo);
		return vo;
	}

	private AwardAmountTransaction prepareAwardAmountTransaction(AwardAmountInfo awardAmountInfoPassed) {
		AwardAmountTransaction awardAmountTransaction = new AwardAmountTransaction();
		awardAmountTransaction.setSourceAwardNumber(awardAmountInfoPassed.getAwardAmountTransaction().getSourceAwardNumber());
		awardAmountTransaction.setDestinationAwardNumber(awardAmountInfoPassed.getAwardAmountTransaction().getDestinationAwardNumber());
		awardAmountTransaction.setNoticeDate(awardAmountInfoPassed.getAwardAmountTransaction().getNoticeDate());
		awardAmountTransaction.setComments(awardAmountInfoPassed.getAwardAmountTransaction().getComments());
		awardAmountTransaction.setAwardTransactionType(awardAmountInfoPassed.getAwardAmountTransaction().getAwardTransactionType());
		awardAmountTransaction.setTransactionTypeCode(awardAmountInfoPassed.getAwardAmountTransaction().getTransactionTypeCode());
		awardAmountTransaction.setFundingProposalNumber(awardAmountInfoPassed.getAwardAmountTransaction().getFundingProposalNumber());
		awardAmountTransaction.setFundedProposalId(awardAmountInfoPassed.getAwardAmountTransaction().getFundedProposalId());
		if (awardAmountInfoPassed.getAwardAmountTransaction().getTransactionId() != null) {
			awardAmountTransaction.setTransactionId(awardAmountInfoPassed.getAwardAmountTransaction().getTransactionId());
			awardAmountTransaction.setUpdateUser(awardAmountInfoPassed.getAwardAmountTransaction().getUpdateUser());
			awardAmountTransaction.setUpdateTimestamp(awardAmountInfoPassed.getAwardAmountTransaction().getUpdateTimestamp());
		} else {
			awardAmountTransaction.setTransactionId(generateTransactionID());
			awardAmountTransaction.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardAmountTransaction.setUpdateUser(awardAmountInfoPassed.getUpdateUser());
		}
		awardAmountTransaction.setAwardNumber(awardAmountInfoPassed.getAwardNumber());
		awardAmountTransaction.setTransactionStatusCode(Constants.PENDING_TRANSACTION);
		awardAmountTransaction.setAwardTransactionStatus(datesAndAmountDao.fetchAwardTransactionStatusById(Constants.PENDING_TRANSACTION));
		awardAmountTransaction = datesAndAmountDao.saveOrUpdateAwardAmountTransaction(awardAmountTransaction);
		return awardAmountTransaction;
	}

	private void saveAwardAmountInfo(AwardDatesandAmountVO vo) {
		AwardAmountInfo awardAmountInfo = vo.getAwardAmountInfo();
		AwardAmountInfo awardLatestAmountInfo = datesAndAmountDao.getLatestAwardAmountInfo(vo.getAwardAmountInfo().getAwardNumber());
		if (awardLatestAmountInfo != null && !vo.isUnRelatedTransaction()) {
			if ((vo.isInternalTransaction() || vo.isExternalTransaction() || vo.isDestinationAccount()) && (!vo.isCanAddTotal())) {
				awardAmountInfo.setAnticipatedTotalAmount(awardLatestAmountInfo.getAnticipatedTotalAmount().add(awardAmountInfo.getAnticipatedChange()));
				awardAmountInfo.setAmountObligatedToDate(awardLatestAmountInfo.getAmountObligatedToDate().add(awardAmountInfo.getObligatedChange()));
			} else if (vo.isCanAddTotal() && !vo.isDestinationAccount()) {
				awardAmountInfo.setAnticipatedTotalAmount(awardLatestAmountInfo.getAnticipatedTotalAmount().subtract(awardAmountInfo.getAnticipatedChange()));
				awardAmountInfo.setAmountObligatedToDate(awardLatestAmountInfo.getAmountObligatedToDate().subtract(awardAmountInfo.getObligatedChange()));
			} else {
				awardAmountInfo.setAnticipatedTotalAmount(awardLatestAmountInfo.getAnticipatedTotalAmount());
				awardAmountInfo.setAmountObligatedToDate(awardLatestAmountInfo.getAmountObligatedToDate());
			}
			if (vo.isDestinationAccount()) {
				awardAmountInfo.setAntDistributableAmount(awardLatestAmountInfo.getAntDistributableAmount().add(awardAmountInfo.getAnticipatedChange()));
				awardAmountInfo.setObliDistributableAmount(awardLatestAmountInfo.getObliDistributableAmount().add(awardAmountInfo.getObligatedChange()));
			} else if (vo.isExternalTransaction() || vo.isInternalTransaction()) {
				awardAmountInfo.setAntDistributableAmount(awardLatestAmountInfo.getAntDistributableAmount());
				awardAmountInfo.setObliDistributableAmount(awardLatestAmountInfo.getObliDistributableAmount());
			} else {
				awardAmountInfo.setAntDistributableAmount(awardLatestAmountInfo.getAntDistributableAmount().subtract(awardAmountInfo.getAnticipatedChange()));
				awardAmountInfo.setObliDistributableAmount(awardLatestAmountInfo.getObliDistributableAmount().subtract(awardAmountInfo.getObligatedChange()));
			}
			awardAmountInfo.setTotalCostInCurrency(awardLatestAmountInfo.getTotalCostInCurrency());
			awardAmountInfo.setCurrencyCode(awardLatestAmountInfo.getCurrencyCode());
			awardAmountInfo.setCurrency(awardLatestAmountInfo.getCurrency());
		} else if (awardLatestAmountInfo != null && vo.isUnRelatedTransaction()) {
			if (vo.isAwardNumberFoundInBoth()) {
				awardAmountInfo.setAnticipatedTotalAmount(awardLatestAmountInfo.getAnticipatedTotalAmount());
				awardAmountInfo.setAmountObligatedToDate(awardLatestAmountInfo.getAmountObligatedToDate());
				awardAmountInfo.setAntDistributableAmount(awardLatestAmountInfo.getAntDistributableAmount());
				awardAmountInfo.setObliDistributableAmount(awardLatestAmountInfo.getObliDistributableAmount());
			} else if (vo.isAwardNumberFoundInSource()) {
				if (vo.isSourceAccount()) {
					awardAmountInfo.setAnticipatedTotalAmount(awardLatestAmountInfo.getAnticipatedTotalAmount().subtract(awardAmountInfo.getAnticipatedChange()));
					awardAmountInfo.setAmountObligatedToDate(awardLatestAmountInfo.getAmountObligatedToDate().subtract(awardAmountInfo.getObligatedChange()));
					awardAmountInfo.setAntDistributableAmount(awardLatestAmountInfo.getAntDistributableAmount().subtract(awardAmountInfo.getAnticipatedChange()));
					awardAmountInfo.setObliDistributableAmount(awardLatestAmountInfo.getObliDistributableAmount().subtract(awardAmountInfo.getObligatedChange()));
				} else {
					awardAmountInfo.setAnticipatedTotalAmount(awardLatestAmountInfo.getAnticipatedTotalAmount().subtract(awardAmountInfo.getAnticipatedChange()));
					awardAmountInfo.setAmountObligatedToDate(awardLatestAmountInfo.getAmountObligatedToDate().subtract(awardAmountInfo.getObligatedChange()));
					awardAmountInfo.setAntDistributableAmount(awardLatestAmountInfo.getAntDistributableAmount());
					awardAmountInfo.setObliDistributableAmount(awardLatestAmountInfo.getObliDistributableAmount());
				}
			} else if (vo.isAwardNumberFoundInDestination()) {
				if (vo.isDestinationAccount()) {
					awardAmountInfo.setAnticipatedTotalAmount(awardLatestAmountInfo.getAnticipatedTotalAmount().add(awardAmountInfo.getAnticipatedChange()));
					awardAmountInfo.setAmountObligatedToDate(awardLatestAmountInfo.getAmountObligatedToDate().add(awardAmountInfo.getObligatedChange()));
					awardAmountInfo.setAntDistributableAmount(awardLatestAmountInfo.getAntDistributableAmount().add(awardAmountInfo.getAnticipatedChange()));
					awardAmountInfo.setObliDistributableAmount(awardLatestAmountInfo.getObliDistributableAmount().add(awardAmountInfo.getObligatedChange()));
				} else {
					awardAmountInfo.setAnticipatedTotalAmount(awardLatestAmountInfo.getAnticipatedTotalAmount().add(awardAmountInfo.getAnticipatedChange()));
					awardAmountInfo.setAmountObligatedToDate(awardLatestAmountInfo.getAmountObligatedToDate().add(awardAmountInfo.getObligatedChange()));
					awardAmountInfo.setAntDistributableAmount(awardLatestAmountInfo.getAntDistributableAmount());
					awardAmountInfo.setObliDistributableAmount(awardLatestAmountInfo.getObliDistributableAmount());
				}
			}
			awardAmountInfo.setTotalCostInCurrency(awardLatestAmountInfo.getTotalCostInCurrency());
			awardAmountInfo.setCurrencyCode(awardLatestAmountInfo.getCurrencyCode());
			awardAmountInfo.setCurrency(awardLatestAmountInfo.getCurrency());
		} else {
			awardAmountInfo.setAnticipatedTotalAmount(awardAmountInfo.getAnticipatedChange());
			awardAmountInfo.setAmountObligatedToDate(awardAmountInfo.getObligatedChange());
			if (vo.isExternalTransaction() || vo.isInternalTransaction()) {
				awardAmountInfo.setAntDistributableAmount(BigDecimal.ZERO);
				awardAmountInfo.setObliDistributableAmount(BigDecimal.ZERO);
			} else {
				awardAmountInfo.setAntDistributableAmount(awardAmountInfo.getAnticipatedChange());
				awardAmountInfo.setObliDistributableAmount(awardAmountInfo.getObligatedChange());
			}
		}
		if (awardAmountInfo.getUpdateTimestamp() != null) {
			awardAmountInfo.setUpdateTimestamp(awardAmountInfo.getUpdateTimestamp());
		} else {
			awardAmountInfo.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		}
		awardAmountInfo = datesAndAmountDao.saveOrUpdateAwardAmountInfo(awardAmountInfo);
		awardService.updateAwardDocumentUpdateUserAndTimestamp(awardAmountInfo.getAwardId(), awardAmountInfo.getUpdateUser());
	}

	public BigDecimal generateTransactionID() {
		DateFormat dateFormat = new SimpleDateFormat("yyMMdd");
		Date date = new Date();
		String dt = String.valueOf(dateFormat.format(date));
		SimpleDateFormat time = new SimpleDateFormat("HHmmssSSS");
		String tm = String.valueOf(time.format(new Date()));
		String id = dt + tm;
		return new BigDecimal(id);
	}

	@Override
	public String getAwardCostShare(AwardVO awardVO) {
		awardVO.setCostShareTypes(commonDao.fetchAllCostShareTypes());
		awardVO.setAwardCostShares(datesAndAmountDao.getCostShareTypesByAwardId(awardVO.getAwardId()));
		return commonDao.convertObjectToJSON(awardVO);
	}

	@Override
	public String getAwardFunds(AwardVO awardVO) {
		Integer awardId = awardVO.getAwardId();
		Award award = awardDao.getAwardDetailsById(awardId);
		BigDecimal sponsorTotalAmount = BigDecimal.ZERO;
		BigDecimal costShareDist = BigDecimal.ZERO;
		BigDecimal instituteTotalAmount = BigDecimal.ZERO;
		AwardFundsDTO awardFundDTO = new AwardFundsDTO();
		List<AwardCostShare> awardCostShares = datesAndAmountDao.getCostShareTypesByAwardId(award.getAwardId());
		if (awardCostShares != null && !awardCostShares.isEmpty()) {
			for (AwardCostShare awardCostShare : awardCostShares) {
				if (awardCostShare.getCommitmentAmount() != null) {
					if (awardCostShare.getSource() != null && awardCostShare.getSource().equals(Constants.SPONSOR)) {
						sponsorTotalAmount = sponsorTotalAmount.add(awardCostShare.getCommitmentAmount());
					} else {
						instituteTotalAmount = instituteTotalAmount.add(awardCostShare.getCommitmentAmount());
					}
				}
				if (awardCostShare.getCostShareDistributable() != null) {
					costShareDist = costShareDist.add(awardCostShare.getCostShareDistributable());
				}
			}
		}
		awardFundDTO.setSponsorTotal(sponsorTotalAmount);
		awardFundDTO.setInstituteTotal(instituteTotalAmount);
		awardFundDTO.setCostShareTotal(sponsorTotalAmount.add(instituteTotalAmount));
		awardFundDTO.setCostShareDistributable(costShareDist);
		awardFundDTO.setProjectStartDate(award.getBeginDate());
		awardFundDTO.setProjectEndDate(award.getFinalExpirationDate());
		if (award != null) {
			awardFundDTO.setPendingAmountInfo(prepareAwardFunds(award, false, awardFundDTO.getCostShareTotal()));
			awardFundDTO.setActiveAmountInfo(prepareAwardFunds(award, true,awardFundDTO.getCostShareTotal()));
		}
		awardFundDTO.setCurrencyDetails(commonDao.fetchCurrencyDetails());
		return commonDao.convertObjectToJSON(awardFundDTO);
	}

	private AwardFunds prepareAwardFunds(Award award, Boolean isActive, BigDecimal costShareTotal) {
		AwardAmountInfo awardAmountInfo = null;
		if (Boolean.TRUE.equals(isActive)) {
			awardAmountInfo = getLatestActiveAwardAmountInfo(award);
		} else {
			awardAmountInfo = getLatestPendingAwardAmountInfo(award);
		}
		if (awardAmountInfo != null) {
			AwardFunds awardFundDTO = new AwardFunds();
			if (awardAmountInfo.getAnticipatedTotalAmount() != null) {
				awardFundDTO.setAnticipatedTotal(awardAmountInfo.getAnticipatedTotalAmount());
			}
			if (awardAmountInfo.getAntDistributableAmount() != null) {
				awardFundDTO.setAnticipatedDistributableTotal(awardAmountInfo.getAntDistributableAmount());
			}
			if (awardAmountInfo.getObliDistributableAmount() != null) {
				awardFundDTO.setObligatedDistributableTotal(awardAmountInfo.getObliDistributableAmount());
			}
			if (awardAmountInfo.getAmountObligatedToDate() != null) {
				awardFundDTO.setObligatedTotal(awardAmountInfo.getAmountObligatedToDate());
			}
			if (awardAmountInfo.getTotalCostInCurrency() != null) {
				awardFundDTO.setTotalCostInCurrency(awardAmountInfo.getTotalCostInCurrency());
			}
			if (awardAmountInfo.getCurrencyCode() != null) {
				awardFundDTO.setCurrencyCode(awardAmountInfo.getCurrencyCode());
				awardFundDTO.setCurrency(awardAmountInfo.getCurrency());
			}
			awardFundDTO.setStartDate(awardAmountInfo.getCurrentFundEffectiveDate());
			awardFundDTO.setEndDate(awardAmountInfo.getObligationExpirationDate());
			awardFundDTO.setTotalCost(costShareTotal.add(awardFundDTO.getAnticipatedTotal()));
			return awardFundDTO;
		} else {
			return null;
		}
	}

	private AwardAmountInfo getLatestPendingAwardAmountInfo(Award award) {
		List<AwardAmountInfo> awardAmountInfos = getAwardAmountInfoBasedOnParams(award.getAwardId(),
				award.getAwardNumber(), award.getSequenceNumber(), award.getAwardSequenceStatus());
		if (awardAmountInfos != null && !awardAmountInfos.isEmpty()) {
			awardAmountInfos = awardAmountInfos.stream()
					.filter(awardAmountInfo -> awardAmountInfo.getAwardAmountTransaction() != null
							&& awardAmountInfo.getAwardAmountTransaction().getTransactionStatusCode()
									.equals(Constants.PENDING_TRANSACTION))
					.sorted(Comparator.comparing(AwardAmountInfo::getAwardAmountInfoId).reversed())
					.collect(Collectors.toList());
		}
		if (awardAmountInfos != null && !awardAmountInfos.isEmpty()) {
			return awardAmountInfos.get(0);
		} else
			return null;
	}

	private AwardAmountInfo getLatestActiveAwardAmountInfo(Award award) {
		List<AwardAmountInfo> awardAmountInfos = getAwardAmountInfoBasedOnParams(award.getAwardId(),
				award.getAwardNumber(), award.getSequenceNumber(), award.getAwardSequenceStatus());
		if (awardAmountInfos != null && !awardAmountInfos.isEmpty()) {
			awardAmountInfos = awardAmountInfos.stream()
					.filter(awardAmountInfo -> awardAmountInfo.getAwardAmountTransaction() != null
							&& awardAmountInfo.getAwardAmountTransaction().getTransactionStatusCode()
									.equals(Constants.ACTIVE_TRANSACTION))
					.sorted(Comparator.comparing(AwardAmountInfo::getAwardAmountInfoId).reversed())
					.collect(Collectors.toList());
		}
		if (awardAmountInfos != null && !awardAmountInfos.isEmpty()) {
			return awardAmountInfos.get(0);
		} else
			return null;
	}

	@Override
	public AwardAmountInfo createAwardAmountInfo(Award award, InstituteProposal instituteProposal, BudgetHeader budgetHeader, String updateUser) {
		AwardAmountTransaction awardAmountTransaction = new AwardAmountTransaction();
		awardAmountTransaction.setTransactionId(generateTransactionID());
		awardAmountTransaction.setTransactionTypeCode(17);
		awardAmountTransaction.setAwardTransactionType(awardDao.getAwardTransactionTypeById(17));
		awardAmountTransaction.setFundedProposalId(instituteProposal.getProposalId());
		awardAmountTransaction.setSourceAwardNumber(Constants.EXTERNAL);
		awardAmountTransaction.setDestinationAwardNumber(award.getAwardNumber());
		awardAmountTransaction.setFundingProposalNumber(instituteProposal.getProposalNumber());
		awardAmountTransaction.setNoticeDate(instituteProposal.getSubmissionDate());
		awardAmountTransaction.setUpdateUser(updateUser);
		awardAmountTransaction.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		awardAmountTransaction.setAwardNumber(award.getAwardNumber());
		awardAmountTransaction.setTransactionStatusCode(Constants.PENDING_TRANSACTION);
		awardAmountTransaction.setAwardTransactionStatus(datesAndAmountDao.fetchAwardTransactionStatusById(Constants.PENDING_TRANSACTION));
		datesAndAmountDao.saveOrUpdateAwardAmountTransaction(awardAmountTransaction);
		AwardAmountInfo awardAmountInfoDetail = new AwardAmountInfo();
		awardAmountInfoDetail.setAwardId(award.getAwardId());
		awardAmountInfoDetail.setAwardNumber(award.getAwardNumber());
		awardAmountInfoDetail.setSequenceNumber(award.getSequenceNumber());
		awardAmountInfoDetail.setTransactionId(awardAmountTransaction.getTransactionId());
		awardAmountInfoDetail.setAwardAmountTransaction(awardAmountTransaction);
		awardAmountInfoDetail.setAnticipatedChange(budgetHeader.getTotalCost());
		awardAmountInfoDetail.setObligatedChange(budgetHeader.getTotalCost());
		awardAmountInfoDetail.setAnticipatedTotalAmount(budgetHeader.getTotalCost());
		awardAmountInfoDetail.setAmountObligatedToDate(budgetHeader.getTotalCost());
		awardAmountInfoDetail.setObliDistributableAmount(budgetHeader.getTotalCost());
		awardAmountInfoDetail.setAntDistributableAmount(budgetHeader.getTotalCost());
		awardAmountInfoDetail.setCurrentFundEffectiveDate(instituteProposal.getStartDate());
		awardAmountInfoDetail.setObligationExpirationDate(instituteProposal.getEndDate());
		awardAmountInfoDetail.setUpdateUser(updateUser);
		awardAmountInfoDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		return datesAndAmountDao.saveOrUpdateAwardAmountInfo(awardAmountInfoDetail);
	}

	@Override
	public String saveTotalProjectCostInForeignCurrency(AwardDatesandAmountVO vo) {
		AwardAmountInfo awardAmountInfo = datesAndAmountDao.getLatestPendingAwardAmountInfo(vo.getAwardNumber());
		if (awardAmountInfo != null) {
			awardAmountInfo.setTotalCostInCurrency(vo.getTotalCostInCurrency());
			awardAmountInfo.setCurrencyCode(vo.getCurrencyCode());
			awardAmountInfo.setCurrency(commonDao.getCurrencyByCurrencyCode(vo.getCurrencyCode()));
		}
		AwardVO awardVO = new AwardVO();
		awardVO.setAwardId(vo.getAwardId());
		return getAwardFunds(awardVO);
	}

	@Override
	public void updateDatesAndAmounts(Integer awardId, String awardNumber, Integer sequenceNumber, String transactionStatus) {
		if (awardDao.getAwardDocumentTypeCode(awardId).equals(Constants.AWARD_SETUP) || Boolean.TRUE.equals(awardDao.isDatesAndAmountEditable(awardId))) {
			datesAndAmountDao.updateDatesAndAmounts(awardId, awardNumber, transactionStatus);
		}
		addToAwardAmountTransactionHistory(awardId, awardNumber, sequenceNumber, transactionStatus);
	}

	private void addToAwardAmountTransactionHistory(Integer awardId, String awardNumber, Integer sequenceNumber, String transactionStatus) {
		AwardAmountInfo latestAwardAmountInfo = datesAndAmountDao.getUsableAwardAmountInfo(awardNumber, transactionStatus);
		if (latestAwardAmountInfo != null) {
			AwardAmountTransactionHistory awardAmountTransactionHistory = new AwardAmountTransactionHistory();
			awardAmountTransactionHistory.setAwardId(awardId);
			awardAmountTransactionHistory.setAwardNumber(awardNumber);
			awardAmountTransactionHistory.setSequenceNumber(sequenceNumber);
			awardAmountTransactionHistory.setTransactionId(latestAwardAmountInfo.getTransactionId());
			awardAmountTransactionHistory.setUpdateUser(AuthenticatedUser.getLoginUserName());
			awardAmountTransactionHistory.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			datesAndAmountDao.saveOrUpdateAwardAmountTransactionHistory(awardAmountTransactionHistory);
		}
	}

	@Override
	public String deleteTransactionDetails(BigDecimal transactionId) {
		try {
			datesAndAmountDao.deleteTransactionsBasedOnTransactionId(transactionId);
			datesAndAmountDao.deleteAwardAmountTransaction(transactionId);
			return commonDao.convertObjectToJSON("SUCESS");
		} catch (Exception e) {
			return commonDao.convertObjectToJSON("ERROR");
		}	
	}

	@Override
	public String saveOrUpdateAnticipatedDistribution(List<AwardAmountFNADistribution> awardAmountFNADistributions) {
		awardAmountFNADistributions.forEach(awardAmountFNADistribution -> {
			datesAndAmountDao.saveOrUpdateAwardAmountFNADistribution(awardAmountFNADistribution);
		});
		return commonDao.convertObjectToJSON(awardAmountFNADistributions);
	}

	@Override
	public String loadAnticipatedDistribution(AwardAmountFNADistributionDTO awardAmountFNADistributionDTO) {
		List<AwardAmountFNADistribution> awardAmountFNADistributions = null;
		if (awardAmountFNADistributionDTO.getTransactionStatus().equals("A") && awardAmountFNADistributionDTO.getSequenceNumber().equals(0)) {
			awardAmountFNADistributions = datesAndAmountDao.getAwardAmountFNADistributionBasedOnAwardId(awardAmountFNADistributionDTO.getAwardId());
		} else if (awardAmountFNADistributionDTO.getSequenceNumber().equals(0) && awardAmountFNADistributionDTO.getTransactionStatus().equals("P")) {
			awardAmountFNADistributions = datesAndAmountDao.getAwardAmountFNADistributionBasedOnAwardId(datesAndAmountDao.getLatestAwardNumber(awardAmountFNADistributionDTO.getAwardNumber()));
			if (awardAmountFNADistributions.isEmpty()) {
				awardAmountFNADistributions = datesAndAmountDao.getAwardAmountFNADistributionBasedOnAwardId(awardAmountFNADistributionDTO.getAwardId());
			}
		} else {
			awardAmountFNADistributions = datesAndAmountDao.getAwardAmountFNADistributionBasedOnAwardId(awardAmountFNADistributionDTO.getAwardId());
			if (awardAmountFNADistributions.isEmpty() && awardAmountFNADistributionDTO.getAwardSequenceStatus().equals(Constants.AWARD_FINAL_STATUS_PENDING)) {
				awardAmountFNADistributions = datesAndAmountDao.getAwardAmountFNADistributionBasedOnAwardId(awardDao.fetchActiveAwardIdByAwardNumber(awardAmountFNADistributionDTO.getAwardNumber()));
			} else {
				Integer lastMergedAwardId =  datesAndAmountDao.getLatestArchiveTransactionAwardBasedOnParam(awardAmountFNADistributionDTO.getAwardId(), awardAmountFNADistributionDTO.getAwardNumber());
				if (lastMergedAwardId != null) {
					awardAmountFNADistributions = datesAndAmountDao.getAwardAmountFNADistributionBasedOnAwardId(lastMergedAwardId);
				}
			}
		}
		return commonDao.convertObjectToJSON(awardAmountFNADistributions);
	}

	@Override
	public String deleteAnticipatedDistribution(Integer fnaDistributionId) {
		try {
			datesAndAmountDao.deleteAwardAmountFNADistribution(datesAndAmountDao.fetchAwardAmountFNADistributionById(fnaDistributionId));
			return commonDao.convertObjectToJSON("Anticipated distribution deleted sucessfully");
		} catch (Exception e) {
			return commonDao.convertObjectToJSON("Error occured while deleting Anticipated distribution");
		}
	}
}
