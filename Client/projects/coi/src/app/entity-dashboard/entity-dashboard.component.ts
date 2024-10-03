import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { deepCloneObject, isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { GraphDetail } from 'projects/shared/src/lib/graph/interface';
import { Subject, Subscription } from 'rxjs';
import { switchMap } from 'rxjs/operators';
import { COMMON_ERROR_TOAST_MSG, ENTITY_DOCUMNET_STATUS_TYPE, ENTITY_VERIFICATION_STATUS, HTTP_ERROR_STATUS } from '../app-constants';
import { CommonService } from '../common/services/common.service';
import { ElasticConfigService } from '../common/services/elastic-config.service';
import { NavigationService } from '../common/services/navigation.service';
import { getEndPointOptionsForCountry } from '../configuration/form-builder-create/shared/form-builder-view/search-configurations';
import { SfiService } from '../disclosure/sfi/sfi.service';
import { EntityDashboardService, EntityDashDefaultValues, SortCountObj, EntityDashboardSearchRequest } from './entity-dashboard.service';
import { slideInOut } from 'projects/fibi/src/app/common/utilities/animations';
import { listAnimation, topSlideInOut, fadeInOutHeight, slideInAnimation, scaleOutAnimation } from '../common/utilities/animations';
import { isValidEmailAddress } from '../common/utilities/custom-utilities';

@Component({
  selector: 'app-entity-dashboard-component',
  templateUrl: './entity-dashboard.component.html',
  styleUrls: ['./entity-dashboard.component.scss'],
  animations: [slideInOut, listAnimation, topSlideInOut, fadeInOutHeight,
    slideInAnimation('0', '12px', 400, 'slideUp'),
    slideInAnimation('0', '-12px', 400, 'slideDown'),
    scaleOutAnimation('-2px', '0', 200, 'scaleOut'),
  ],
  providers: [EntityDashboardService]
})

export class EntityDashboardComponent {
  graphEvent: Subject<GraphDetail> = new Subject<GraphDetail>();
  activeTabName = 'ALL_ENTITY';
  isViewAdvanceSearch = false;
  $coiList = new Subject();
  coiList = [];
  isCoiEditEntity = false;
  clearField: String;
  advanceSearchDates = { certificationDate: null, expirationDate: null };
  advSearchClearField: String;
  EntitySearchOptions: any = {};
  countrySearchOptions: any = {};
  lookupValues = [];
  entityTypeOptions = 'entity_type#ENTITY_TYPE_CODE#true#true';
  statusTypeOptions = 'entity_document_status_type#DOCUMENT_STATUS_TYPE_CODE#true#true#true#true';
  verificationTypeOptions = 'entity_status#ENTITY_STATUS_CODE#true#true';
  ownershipTypeOptions = 'entity_ownership_type#OWNERSHIP_TYPE_CODE#true#true';
  $entityList = new Subject();
  entityList: any = [];
  $subscriptions: Subscription[] = [];
  resultCount = 0;
  isShowEntityList = false;
  isShowAllProposalList = false;
  rightList: string;
  canViewEntity: boolean;
  result: any;
  isActiveDisclosureAvailable: boolean;
  isLoading = false;
  isShowGraph = false;
  isManageEntity = false;
  sortSectionsList = [
    { variableName: 'PRIMARY_NAME', fieldName: 'Name' },
    { variableName: 'COUNTRY', fieldName: 'Country' },
    { variableName: 'OWNERSHIP_TYPE', fieldName: 'Ownership Type' },
    { variableName: 'ENTITY_STATUS', fieldName: 'Entity Status' },
    { variableName: 'UPDATE_TIMESTAMP', fieldName: 'Last Updated' }
];
  mandatoryList = new Map();

  sortMap: any = {};
  sortCountObj: any = {};
  tempEntitySearchRequestObject = new EntityDashboardSearchRequest();
  advanceSearchProperties:any = {};
  clearEntityName: String;
  isSearchFormInvalid: boolean = false;
  canModifyEntity: boolean;
  ENTITY_ACTIVE = ENTITY_DOCUMNET_STATUS_TYPE.ACTIVE;
  ENTITY_DUPLICATE = ENTITY_DOCUMNET_STATUS_TYPE.DUPLICATE;
  ENTITY_INACTIVE = ENTITY_DOCUMNET_STATUS_TYPE.INACTIVE;
  ENTITY_VERIFIED = ENTITY_VERIFICATION_STATUS.VERIFIED;
  ENTITY_UNVERIFIED = ENTITY_VERIFICATION_STATUS.UNVERIFIED;

  constructor(private _router: Router,
    public entityDashboardService: EntityDashboardService,
    private _navigationService: NavigationService,
    private _commonService: CommonService, public sfiService: SfiService,
    private _elasticConfig: ElasticConfigService
) { }

  ngOnInit() {
    const entityDashboardData = this.entityDashboardService.entitySearchRequestObject.entityDashboardData;
    this.checkForSort();
    this.EntitySearchOptions = this._elasticConfig.getElasticForEntity();
    this.countrySearchOptions = getEndPointOptionsForCountry(this._commonService.fibiUrl);
    this.isViewAdvanceSearch = false;
    if (!entityDashboardData.TAB_TYPE) {
      entityDashboardData.TAB_TYPE = this.activeTabName;
    } else {
      this.activeTabName = entityDashboardData.TAB_TYPE;
      this.EntitySearchOptions.defaultValue = this.entityDashboardService.entityDashDefaultValues.entitySearch;
      this.countrySearchOptions.defaultValue = this.entityDashboardService.entityDashDefaultValues.countrySearch;
      this.generateLookupArrayForDropdown();
    }
    this.checkUserHasRight();
    this.setSortProperties();
    this.viewListOfEntity();
    this.checkForAdvanceSearch();
    this.showEntities();
    this.isShowGraph = this._commonService.enableGraph;
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
    this.entityDashboardService.isShowEntityNavBar = false;
  }

  entityTabName(tabName: string) {
    this.tempEntitySearchRequestObject = new EntityDashboardSearchRequest();
    this.resetAdvanceSearchFields();
    this.entityList = [];
    this.resultCount = null;
    this.activeTabName = tabName;
    this.entityDashboardService.entitySearchRequestObject.entityDashboardData.TAB_TYPE = this.activeTabName;
    this.isShowAllProposalList = true;
    this.entityDashboardService.isAdvanceSearch = false;
    if (this.activeTabName === 'ALL_ENTITY') {
        this.isShowEntityList = false;
      this.isViewAdvanceSearch = true;
    } else {
      this.isShowEntityList = true;
      this.$entityList.next();
      this.isViewAdvanceSearch = false;
      this.isShowAllProposalList = true;
    }
  }

  checkForAdvanceSearch() {
    if ( this.entityDashboardService.isAdvanceSearch) {
       this.isViewAdvanceSearch = true;
    } else {
        this.entityDashboardService.entitySearchRequestObject = new EntityDashboardSearchRequest();
    }
    if (this.activeTabName === 'ALL_ENTITY') {
      this.isViewAdvanceSearch = true;
      this.isShowEntityList = false;
      if (this.entityDashboardService.isAdvanceSearch) {
        this.isShowEntityList = true;
        this.$entityList.next();
      }
    }
  }

  redirectToEntity(coi: any) {
    this._router.navigate(['/coi/entity-management/entity-details'], { queryParams: { entityManageId: coi.id } });
  }

 viewListOfEntity() {
    this.$subscriptions.push(this.$entityList.pipe(
      switchMap(() => {
        this.isLoading = true;
        return this.entityDashboardService.getAllSystemEntityList(this.getDashboardRO());

      })).subscribe((res: any) => {
        this.isShowEntityList = true;
        this.entityList = res.dashboardResponses || [];
        this.resultCount = res.totalEntityResponse;
        this.isLoading = false;
      }, _error => {
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        this.entityList = [];
        this.isLoading = false;
      }));
  }

  getDashboardRO() {
		let RO: any = deepCloneObject(this.entityDashboardService.entitySearchRequestObject);
		if(!Object.keys(RO.entityDashboardData.SORT_TYPE).length) {
			delete RO.entityDashboardData.SORT_TYPE;
		} else {
			RO.entityDashboardData.SORT_TYPE = this.getSortType(RO.entityDashboardData.SORT_TYPE);
		}
		return RO;
	}

	getSortType(obj): string {
		const keyValuePairs = Object.entries(obj)
			.map(([key, value]) => `${key} ${value}`)
			.join(', ');
		return keyValuePairs;
	}

  addNewEntity() {
    this.isCoiEditEntity = false;
    this._router.navigate(['/coi/manage-entity/create-entity']);
  }

  setEntityNameForAdvanceSearch(event) {
		event ? this.tempEntitySearchRequestObject.PRIMARY_NAME = event.entity_name : delete this.tempEntitySearchRequestObject.PRIMARY_NAME;
        event ? this.tempEntitySearchRequestObject.grantCallName = event.entity_name : delete this.tempEntitySearchRequestObject.grantCallName;
	}

  clearEntityNameForAdvanceSearch() {
		delete this.entityDashboardService.entitySearchRequestObject.PRIMARY_NAME;
		this.clearEntityName = new String('true');
	}

  showEntities() {
    if (this.activeTabName !== 'ALL_ENTITY' ) {
      this.$entityList.next();
      this.isShowEntityList = true;
      this.isShowAllProposalList = true;
      if (this.isViewAdvanceSearch) {
       this. isViewAdvanceSearch = true;
      }
    }
  }

  selectEntityCountry(event) {
		event ? this.tempEntitySearchRequestObject.COUNTRY = event.countryCode : delete this.tempEntitySearchRequestObject.COUNTRY;
		event ? this.tempEntitySearchRequestObject.countryName = event.countryName : delete this.tempEntitySearchRequestObject.countryName;
	}

  onLookupSelect(data, template) {
		this.lookupValues[template] = data;
		if (data.length) {
			this.tempEntitySearchRequestObject[template] = data.map(d => d.code);
		} else {
			delete this.tempEntitySearchRequestObject[template];
		}
	}

  resetAdvanceSearchFields() {
    this.clearEntityNameForAdvanceSearch();
    this.entityDashboardService.entityDashDefaultValues = new EntityDashDefaultValues();
    this.EntitySearchOptions = this._elasticConfig.getElasticForEntity();
    this.countrySearchOptions = getEndPointOptionsForCountry(this._commonService.fibiUrl);
    this.lookupValues = [];
    this.tempEntitySearchRequestObject = new EntityDashboardSearchRequest();
    this.entityDashboardService.entitySearchRequestObject = new EntityDashboardSearchRequest();
    this.isSearchFormInvalid = false;
    this.mandatoryList.clear();
  }

  clearAdvancedSearch() {
    this.resetAdvanceSearchFields();
    this.entityDashboardService.entitySearchRequestObject.entityDashboardData.TAB_TYPE = this.activeTabName;
    this.entityList = [];
    this.$entityList.next();
  }

  advancedSearch() {
    if(this.isSearchFormInvalid) {
      return;
    }
    this.tempEntitySearchRequestObject.entityDashboardData.PAGED = 0;
    this.entityDashboardService.entitySearchRequestObject.entityDashboardData.PAGED = 0;
    this.entityList = [];
    this.switchAdvanceSearchProperties(this.entityDashboardService.entitySearchRequestObject, this.tempEntitySearchRequestObject);
    this.$entityList.next();
    this.isShowEntityList = true;
    this.isShowAllProposalList = true;
    this.entityDashboardService.isAdvanceSearch = true;
    }

    switchAdvanceSearchProperties(destination, source) {
      this.advanceSearchProperties.TAB_TYPE = destination.entityDashboardData.TAB_TYPE;
      this.advanceSearchProperties.SORT_TYPE = destination.entityDashboardData.SORT_TYPE;

      const singleInputFieldKeys = [
        'PRIMARY_NAME',
        'PRIMARY_ADDRESS_LINE_1',
        'PRIMARY_ADDRESS_LINE_2',
        'CITY',
        'STATE',
        'COUNTRY',
        'DUNS_NUMBER',
        'UEI_NUMBER',
        'CAGE_NUMBER',
        'WEBSITE_ADDRESS',
        'CERTIFIED_EMAIL',
        'FOREIGN_NAME',
        'PRIOR_NAME'
      ];
      for (const property of singleInputFieldKeys) {
        if (source[property]) {
          this.advanceSearchProperties[property] = source[property].trim();
        } else {
          delete this.advanceSearchProperties[property];
        }
      }

      const multipleInputFieldKeys = ['ENTITY_STATUS_TYPE_CODE', 'VERIFICATION_STATUS', 'OWNERSHIP_TYPE_CODE'];
      multipleInputFieldKeys.forEach(property => {
        if (source[property]) {
          this.advanceSearchProperties[property] = source[property].join(', ');
        } else {
          delete this.advanceSearchProperties[property];
          delete destination.entityDashboardData[property];
        }
      });

      Object.assign(this.tempEntitySearchRequestObject, this.entityDashboardService.entitySearchRequestObject.entityDashboardData);
      Object.assign(destination.entityDashboardData, this.advanceSearchProperties);
    }

  generateLookupArrayForDropdown() {
    const entityDashboardData = this.entityDashboardService.entitySearchRequestObject.entityDashboardData;
		if (entityDashboardData.ENTITY_STATUS_TYPE_CODE && entityDashboardData.ENTITY_STATUS_TYPE_CODE.length !== 0) {
			this.tempEntitySearchRequestObject['ENTITY_STATUS_TYPE_CODE'] = this.changeStringToArrayForLookups(entityDashboardData.ENTITY_STATUS_TYPE_CODE);
			this.generateLookupArray(entityDashboardData.ENTITY_STATUS_TYPE_CODE, 'ENTITY_STATUS_TYPE_CODE');
		}
		if (entityDashboardData.VERIFICATION_STATUS && entityDashboardData.VERIFICATION_STATUS.length !== 0) {
			this.tempEntitySearchRequestObject['VERIFICATION_STATUS'] = this.changeStringToArrayForLookups(entityDashboardData.VERIFICATION_STATUS);
			this.generateLookupArray(entityDashboardData.VERIFICATION_STATUS, 'VERIFICATION_STATUS');
		}
		if (entityDashboardData.OWNERSHIP_TYPE_CODE && entityDashboardData.OWNERSHIP_TYPE_CODE.length !== 0) {
			this.tempEntitySearchRequestObject['OWNERSHIP_TYPE_CODE'] = this.changeStringToArrayForLookups(entityDashboardData.OWNERSHIP_TYPE_CODE);
			this.generateLookupArray(entityDashboardData.OWNERSHIP_TYPE_CODE, 'OWNERSHIP_TYPE_CODE');
		}
	}

  /**Since we get string as resultant if advance search is made we are changing string to array*/
	changeStringToArrayForLookups(property) {
		let str_arr = [];
		let result_arr = [];
		str_arr = property.split(', ');
		if(str_arr.length) {
			str_arr.forEach(element => {
				result_arr.push(element);
			});
		} else {
			result_arr.push(property);
		}
		return result_arr;
	}

  generateLookupArray(property, propertyNumber) {
		this.lookupValues[propertyNumber] = [];
		let str_arr = [];
		str_arr = property.split(',');
		if(str_arr.length) {
			str_arr.forEach(element => {
				this.lookupValues[propertyNumber].push({ code: element });
			});
		} else {
			this.lookupValues[propertyNumber] = { code: property }
		}
	}

  viewDetails(entityListData) {
    this._router.navigate(['/coi/manage-entity/entity-overview'], { queryParams: { entityManageId: entityListData.entityId} });
  }

    modifyEntity(entityListData) {
        const REQ_OBJ = { entityId: entityListData.entityId, actionLogCode: 15 };
        this.$subscriptions.push(this.entityDashboardService.logFeedHistory(REQ_OBJ).subscribe((data: any) => {
            if (data) {
                this._commonService.isEntityModified = true;
                this.viewDetails(entityListData);
            }
        }, err => {
            this._commonService.showToast(HTTP_ERROR_STATUS, COMMON_ERROR_TOAST_MSG);
        }));
    }

  actionsOnPageChange(event) {
      this.entityDashboardService.entitySearchRequestObject.entityDashboardData.PAGED = event-1;
      this.$entityList.next();
  }

  checkUserHasRight(): void {
    this.canModifyEntity = this._commonService.getAvailableRight(['MANAGE_ENTITY', 'MANAGE_ENTITY_SPONSOR', 'MANAGE_ENTITY_ORGANIZATION', 'MANAGE_ENTITY_COMPLIANCE'], 'SOME');
    this.canViewEntity = (this.canModifyEntity || this._commonService.getAvailableRight(['VIEW_ENTITY','VERIFY_ENTITY'], 'SOME'));
    this.isManageEntity = this._commonService.getAvailableRight(['MANAGE_ENTITY'], 'SOME');
  }

setEventTypeFlag() {
  this.isActiveDisclosureAvailable = !!this.coiList.find((ele: any) => ele.disclosureSequenceStatusCode === '2');
}

  checkForSort() {
    const entityDashboardData = this.entityDashboardService.entitySearchRequestObject.entityDashboardData;
    if (!isEmptyObject(entityDashboardData.SORT_TYPE) &&
      this._navigationService.previousURL.includes('manage-entity/create-entity')) {
      this.sortCountObj = deepCloneObject(this.entityDashboardService.sortCountObject);
    } else {
      this.resetSortObjects();
    }
  }

    toggleAdvanceSearch() {
        this.isViewAdvanceSearch = !this.isViewAdvanceSearch;
        this.entityDashboardService.isAdvanceSearch = this.isViewAdvanceSearch;
    }

    openGraph(entityId, entityName) {
        this.graphEvent.next({ visible: true, id: entityId, name: entityName });
    }

  validateEmail() {
    this.clearValidation('certifiedEmail');
    if (this.tempEntitySearchRequestObject.CERTIFIED_EMAIL && !isValidEmailAddress(this.tempEntitySearchRequestObject.CERTIFIED_EMAIL)) {
      this.mandatoryList.set('certifiedEmail', 'Please enter valid Email.');
      this.isSearchFormInvalid = true;
      this.entityDashboardService.isAdvanceSearch = false;
    } else {
      this.isSearchFormInvalid = false;
      this.entityDashboardService.isAdvanceSearch = true;
      this.mandatoryList.clear();
    }
  }

  clearValidation(type) {
    this.mandatoryList.delete(type);
  }

  sortResult(sortFieldBy) {
    this.sortCountObj[sortFieldBy]++;
    if (this.sortCountObj[sortFieldBy] < 3) {
      this.sortMap[sortFieldBy] = !this.sortMap[sortFieldBy] ? 'ASC' : 'DESC';
    } else {
      this.sortCountObj[sortFieldBy] = 0;
      delete this.sortMap[sortFieldBy];
    }
    this.entityDashboardService.entitySearchRequestObject.entityDashboardData.SORT_TYPE = this.sortMap;
    this.entityDashboardService.sortCountObj = this.sortCountObj;
    this.$entityList.next();
  }

  setSortProperties() {
    this.resetSortObjects();
    this.sortCountObj = (Object.entries(this.entityDashboardService.sortCountObj).length === 0) ?
      this.sortCountObj : this.entityDashboardService.sortCountObj;
    this.sortMap = (Object.entries(this.entityDashboardService.entitySearchRequestObject.entityDashboardData.SORT_TYPE).length === 0) ?
      this.sortMap : this.entityDashboardService.entitySearchRequestObject.entityDashboardData.SORT_TYPE;
  }

  resetSortObjects() {
    this.sortMap = {};
    this.sortCountObj = {
      'PRIMARY_NAME': 0, 'COUNTRY': 0, 'OWNERSHIP_TYPE': 0, 'ENTITY_STATUS': 0, 'UPDATE_TIMESTAMP': 2
    };
  }

  isActive(colName) {
    if (!isEmptyObject(this.sortMap) && colName in this.sortMap) {
      return true;
    } else {
      return false;
    }

  }
}
