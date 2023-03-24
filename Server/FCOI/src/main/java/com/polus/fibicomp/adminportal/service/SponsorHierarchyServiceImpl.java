package com.polus.fibicomp.adminportal.service;

import com.polus.fibicomp.adminportal.dao.SponsorHierarchyDao;
import com.polus.fibicomp.adminportal.dto.SponsorDto;
import com.polus.fibicomp.adminportal.dto.SponsorHierarchyDto;
import com.polus.fibicomp.adminportal.pojo.SponsorHierarchy;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.dto.ResponseData;
import com.polus.fibicomp.search.pojo.SearchResult;
import com.polus.fibicomp.security.AuthenticatedUser;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.*;

@Service(value = "SponsorHierarchyService")
@Transactional
public class SponsorHierarchyServiceImpl implements SponsorHierarchyService {

    protected static Logger logger = LogManager.getLogger(SponsorHierarchyServiceImpl.class.getName());

    @Autowired
    private SponsorHierarchyDao sponsorHierarchyDao;

    @Autowired
    private CommonDao commonDao;

    @Override
    public ResponseEntity<ResponseData> createSponsorHierarchy(SponsorHierarchyDto sponsorHierarchyDto) {
        try {
            SponsorHierarchy sponsorHierarchy = new SponsorHierarchy();
            BeanUtils.copyProperties(sponsorHierarchyDto, sponsorHierarchy, "sponsorGroupId", "childSponsorHierarchies", "sponsor");
            sponsorHierarchy.setCreateUser(AuthenticatedUser.getLoginUserName());
            sponsorHierarchy.setCreateTimestamp(commonDao.getCurrentTimestamp());
            sponsorHierarchy.setUpdateUser(AuthenticatedUser.getLoginUserName());
            sponsorHierarchy.setUpdateTimestamp(commonDao.getCurrentTimestamp());
            sponsorHierarchyDao.saveSponsorHierarchy(sponsorHierarchy);
            if ((sponsorHierarchy.getChildSponsorHierarchies() == null || sponsorHierarchy.getChildSponsorHierarchies()
                    .isEmpty()) && sponsorHierarchy.getSponsorCode() == null)
                sponsorHierarchyDto.setEmptyGroup(true);
            sponsorHierarchyDto.setSponsorOriginatingGroupId(sponsorHierarchy.getSponsorOriginatingGroupId());
            sponsorHierarchyDto.setSponsorGroupId(sponsorHierarchy.getSponsorGroupId());
            sponsorHierarchyDto.setCreateUser(sponsorHierarchy.getCreateUser());
            sponsorHierarchyDto.setCreateTimestamp(sponsorHierarchy.getCreateTimestamp());
            sponsorHierarchyDto.setUpdateUser(sponsorHierarchy.getUpdateUser());
            sponsorHierarchyDto.setUpdateTimestamp(sponsorHierarchy.getUpdateTimestamp());
            return new ResponseEntity<>(new ResponseData(sponsorHierarchyDto, "Successfully Saved",
                    commonDao.getCurrentTimestamp(), true), HttpStatus.OK);
        } catch (Exception e) {
            logger.error(" Exception in createSponsorHierarchy : {}", e.getMessage());
            return new ResponseEntity<>(new ResponseData(null, "Unable to save data", commonDao.getCurrentTimestamp(),
                    false, e.getMessage()), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    @Override
    public ResponseEntity<ResponseData> getSponsorHierarchy(SearchResult searchResult) {
        try {
            if (searchResult == null && searchResult.getSponsorGroupId() != null) {
                return new ResponseEntity<>(new ResponseData(null, "Empty Data", commonDao.getCurrentTimestamp(),
                        false), HttpStatus.BAD_REQUEST);
            }
            List<SponsorHierarchyDto> sponsorHierarchyList = new ArrayList<>();
            SponsorHierarchy sponsorHierarchy = sponsorHierarchyDao.findSponsorHierarchyBySponsorGroupIdOrderByOrderNumberDesc(searchResult.getSponsorGroupId());
            SponsorHierarchyDto sponsorHierarchyDto = new SponsorHierarchyDto();
            BeanUtils.copyProperties(sponsorHierarchy, sponsorHierarchyDto);
            if ((sponsorHierarchy.getChildSponsorHierarchies() == null || sponsorHierarchy.getChildSponsorHierarchies()
                    .isEmpty()) && sponsorHierarchy.getSponsorCode() == null)
                sponsorHierarchyDto.setEmptyGroup(true);
            if (sponsorHierarchy.getChildSponsorHierarchies() != null) {
                sponsorHierarchyToDto(sponsorHierarchyList, sponsorHierarchy.getChildSponsorHierarchies());
                sponsorHierarchyDto.setChildSponsorHierarchies(sponsorHierarchyList);
            }
            return new ResponseEntity<>(new ResponseData(sponsorHierarchyDto, "Success", commonDao.getCurrentTimestamp(),
                    true), HttpStatus.OK);
        } catch (Exception e) {
            logger.error(" Exception in getSponsorHierarchy : {}", e.getMessage());
            return new ResponseEntity<>(new ResponseData(null, "Failed to load data", commonDao.getCurrentTimestamp(),
                    false, e.getMessage()), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    private void sponsorHierarchyToDto(List<SponsorHierarchyDto> sponsorHierarchyList, List<SponsorHierarchy> sponsorHierarchies) {
        sponsorHierarchies.forEach(sponHierObj -> {
            SponsorHierarchyDto sponsorHierarchy = new SponsorHierarchyDto();
            BeanUtils.copyProperties(sponHierObj, sponsorHierarchy, "sponsor", "childSponsorHierarchies");
            if ((sponHierObj.getChildSponsorHierarchies() == null || sponHierObj.getChildSponsorHierarchies().isEmpty()) &&
                    sponHierObj.getSponsorCode() == null)
                sponsorHierarchy.setEmptyGroup(true);
            if (sponHierObj.getSponsor() != null) {
                SponsorDto sponsorDto = new SponsorDto();
                BeanUtils.copyProperties(sponHierObj.getSponsor(), sponsorDto);
                sponsorHierarchy.setSponsor(sponsorDto);
            }
            if (sponHierObj.getChildSponsorHierarchies() != null && !sponHierObj.getChildSponsorHierarchies().isEmpty()) {
                List<SponsorHierarchyDto> sponHierarchies = new ArrayList<>();
                sponsorHierarchyToDto(sponHierarchies, sponHierObj.getChildSponsorHierarchies());
                sponsorHierarchy.setChildSponsorHierarchies(sponHierarchies);
            }
            sponsorHierarchyList.add(sponsorHierarchy);
        });
    }

    @Override
    public ResponseEntity<ResponseData> updateSponsorHierarchy(SponsorHierarchyDto sponsorHierarchyDto) {
        try {
            if (sponsorHierarchyDto != null && sponsorHierarchyDto.getSponsorGroupId() == null) {
                SponsorHierarchy sponHiearachy = new SponsorHierarchy();
                BeanUtils.copyProperties(sponsorHierarchyDto, sponHiearachy);
                sponHiearachy.setCreateUser(AuthenticatedUser.getLoginUserName());
                sponHiearachy.setCreateTimestamp(commonDao.getCurrentTimestamp());
                sponHiearachy.setUpdateUser(AuthenticatedUser.getLoginUserName());
                sponHiearachy.setUpdateTimestamp(commonDao.getCurrentTimestamp());
                sponsorHierarchyDao.saveSponsorHierarchy(sponHiearachy);
                BeanUtils.copyProperties(sponHiearachy, sponsorHierarchyDto);
                if ((sponHiearachy.getChildSponsorHierarchies() == null || sponHiearachy.getChildSponsorHierarchies().isEmpty()) &&
                        sponHiearachy.getSponsorCode() == null)
                    sponsorHierarchyDto.setEmptyGroup(true);
            } else if (sponsorHierarchyDto.getChildSponsorHierarchies() != null && !sponsorHierarchyDto.getChildSponsorHierarchies().isEmpty()) {
                SponsorHierarchy sponHiearachy = sponsorHierarchyDao.findSponsorHierarchyBySponsorGroupIdOrderByOrderNumberDesc(sponsorHierarchyDto.getSponsorGroupId());
                updateSponsorHierarchySponsors(sponsorHierarchyDto.getChildSponsorHierarchies(), sponHiearachy);
            } else {
                sponsorHierarchyDao.updateSponsorHierarchy(sponsorHierarchyDto.getSponsorGroupName(), sponsorHierarchyDto.getOrderNumber(),
                        commonDao.getCurrentTimestamp(), AuthenticatedUser.getLoginUserName(), sponsorHierarchyDto.getSponsorGroupId());
            }
            return new ResponseEntity<>(new ResponseData(sponsorHierarchyDto, "Success", commonDao.getCurrentTimestamp(), true), HttpStatus.OK);
        } catch (Exception e) {
            logger.error(" Exception in updateSponsorHierarchy : {}", e.getMessage());
            return new ResponseEntity<>(new ResponseData(null, "Failed to update data", commonDao.getCurrentTimestamp(),
                    false, e.getMessage()), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    private void updateSponsorHierarchySponsors(List<SponsorHierarchyDto> sponsorHierarchyList, SponsorHierarchy sponHiearachy) {
        for (SponsorHierarchyDto sponHierObj : sponsorHierarchyList) {
            if (sponHierObj.getAccType().equals("D")) {
                sponsorHierarchyDao.deleteSponsorHierarchyById(sponHierObj.getSponsorGroupId());
            } else if (sponHierObj.getAccType().equals("I")) {
                SponsorHierarchy sponsorHierarchy = new SponsorHierarchy();
                sponsorHierarchy.setSponsorOriginatingGroupId(sponHiearachy.getSponsorGroupId());
                sponsorHierarchy.setOrderNumber(sponHierObj.getOrderNumber());
                sponsorHierarchy.setSponsorGroupName(sponHiearachy.getSponsorGroupName());
                sponsorHierarchy.setSponsorRootGroupId(sponHiearachy.getSponsorRootGroupId());
                sponsorHierarchy.setSponsorCode(sponHierObj.getSponsorCode());
                sponsorHierarchy.setCreateUser(AuthenticatedUser.getLoginUserName());
                sponsorHierarchy.setCreateTimestamp(commonDao.getCurrentTimestamp());
                sponsorHierarchy.setUpdateUser(AuthenticatedUser.getLoginUserName());
                sponsorHierarchy.setUpdateTimestamp(commonDao.getCurrentTimestamp());
                sponsorHierarchyDao.saveSponsorHierarchy(sponsorHierarchy);
                sponHierObj.setSponsorGroupId(sponsorHierarchy.getSponsorGroupId());
            }
        }
    }

    @Override
    public ResponseEntity<ResponseData> deleteSponsorHierarchy(Integer sponsorGroupId) {
        try {
            sponsorHierarchyDao.deleteSponsorHierarchyById(sponsorGroupId);
            return new ResponseEntity<>(new ResponseData(null, "Successfully deleted", commonDao.getCurrentTimestamp(),
                    true), HttpStatus.OK);
        } catch (Exception e) {
            logger.error(" Exception in deleteSponsorHierarchy : {}", e.getMessage());
            return new ResponseEntity<>(new ResponseData(null, "Failed to delete", commonDao.getCurrentTimestamp(),
                    false, e.getMessage()), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    @Override
    public ResponseEntity<Object> getNotAddedSponsorsInSH(Integer rootGroupId, String searchWord) {
        try {
            if (rootGroupId == null || searchWord == null || searchWord.isEmpty())
                return new ResponseEntity<>(null, HttpStatus.BAD_REQUEST);
            List<Object> sponsors = sponsorHierarchyDao.findAllSponsorsNotInSponsorHierarchy(rootGroupId,
                    "%" + searchWord + "%");
            List<SponsorDto> sponsorDtos = new ArrayList<>();
            sponsors.forEach(sponsorObj -> {
                Object[] obj = (Object[]) sponsorObj;
                SponsorDto sponsorDto = new SponsorDto();
                sponsorDto.setSponsorCode((String) obj[0]);
                sponsorDto.setSponsorName((String) obj[1]);
                sponsorDto.setSponsorType((String) obj[2]);
                sponsorDto.setAcronym((String) obj[3]);
                sponsorDtos.add(sponsorDto);
            });
            return new ResponseEntity<>(sponsorDtos, HttpStatus.OK);
        } catch (Exception e) {
            logger.error(" Exception in getNotAddedSponsorsInSH : {}", e.getMessage());
            return new ResponseEntity<>(null, HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    @Override
    public ResponseEntity<ResponseData> getSponsorHierarchies() {
        try {
            List<SponsorHierarchyDto> sponsorHierarchyList = new ArrayList<>();
            sponsorHierarchyDao.getAllSponsorHierarchyFromRoot().forEach(sponHierarchy -> {
                SponsorHierarchyDto sponsorHierarchyDto = new SponsorHierarchyDto();
                BeanUtils.copyProperties(sponHierarchy, sponsorHierarchyDto, "childSponsorHierarchies", "sponsor");
                if ((sponHierarchy.getChildSponsorHierarchies() == null || sponHierarchy.getChildSponsorHierarchies().isEmpty()) &&
                        sponHierarchy.getSponsorCode() == null)
                    sponsorHierarchyDto.setEmptyGroup(true);
                sponsorHierarchyList.add(sponsorHierarchyDto);
            });
            return new ResponseEntity<>(new ResponseData(sponsorHierarchyList, "Success", commonDao.getCurrentTimestamp(), true), HttpStatus.OK);
        } catch (Exception e) {
            logger.error(" Exception in getSponsorHierarchies : {}", e.getMessage());
            return new ResponseEntity<>(new ResponseData(null, "Unable to load data", commonDao.getCurrentTimestamp(),
                    false, e.getMessage()), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    @Override
    public ResponseEntity<Object> getAllSponsorHierarchyGroups(String searchWord) {
        try {
            List<Object> resultData;
            if (searchWord == null || searchWord.isEmpty()) {
                resultData = sponsorHierarchyDao.findAllDistinctSponsorGroupName();
            } else {
                resultData = sponsorHierarchyDao.findAllDistinctSponsorGroupNameBySearchWord("%" + searchWord + "%");
            }
            List<Map<String, Object>> responseData = new ArrayList<>();
            for (Object data : resultData) {
                Object[] obj = (Object[]) data;
                Map<String, Object> objectMap = new HashMap<>();
                objectMap.put("sponsorGroupName", obj[0]);
                objectMap.put("sponsorGroupId", obj[1]);
                responseData.add(objectMap);
            }
            return new ResponseEntity<>(responseData, HttpStatus.OK);
        } catch (Exception e) {
            logger.error(" Exception in getAllSponsorHierarchyGroups : {}", e.getMessage());
            return new ResponseEntity<>(null, HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }
}
