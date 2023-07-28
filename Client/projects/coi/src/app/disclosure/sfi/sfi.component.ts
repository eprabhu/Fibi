import { Component, ElementRef, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { Subject, Subscription, interval } from 'rxjs';

import { SfiService } from './sfi.service';
import { DataStoreService } from '../services/data-store.service';
import { CoiService } from '../services/coi.service';
import {subscriptionHandler} from "../../../../../fibi/src/app/common/utilities/subscription-handler";
import { Router } from '@angular/router';
import { CommonService } from '../../common/services/common.service';
import { HTTP_SUCCESS_STATUS, HTTP_ERROR_STATUS } from '../../app-constants';
import { debounce, switchMap } from 'rxjs/operators';
import { GetSFIRequestObject } from '../coi-interface';
import { fadeInOutHeight, leftSlideInOut, listAnimation } from '../../../../../fibi/src/app/common/utilities/animations';

@Component({
    selector: 'app-sfi',
    templateUrl: './sfi.component.html',
    styleUrls: ['./sfi.component.scss'],
    animations: [listAnimation, fadeInOutHeight, leftSlideInOut]
})
export class SfiComponent implements OnInit, OnDestroy {

    @ViewChild('viewSFIDetailsOverlay', { static: true }) viewSFIDetailsOverlay: ElementRef;
    $subscriptions: Subscription[] = [];
    coiFinancialEntityDetails: any[] = [];
    searchText: string;
    searchResult = [];
    dependencies = ['coiDisclosure', 'numberOfSFI'];
    isEditMode = false;
    conflictStatusCode: any;
    disclosureId: any;
    personId: any;
    isSFINotAvailable = false;
    reviewStatus: any;
    filterType = 'ALL';
    currentPage = 1;
    count: any;
    showSlider = false;
    scrollHeight: number;
    entityId: any;
    personEntityId: any;
    entityName: any;
    isRelationshipActive: false;
    entityDetails: any;
    expandInfo = true;
    isEnableActivateInactivateSfiModal: boolean;
    $debounceEvent = new Subject();
    $fetchSFIList = new Subject();
    isSearchTextHover = false;
    isLoading = false;

    constructor(
        private _sfiService: SfiService,
        private _dataStore: DataStoreService,
        public _coiService: CoiService,
        private _router: Router,
        private _commonService: CommonService) {
    }

    ngOnInit() {
        this._coiService.isShowSFIInfo = true;
        this.isLoading = true;
        this.getEditMode();
        this.getSfiDetails();
        this.$fetchSFIList.next();
        this.getSearchList();
        this.listenDataChangeFromStore();
        this.listenForAdd();
    }

    ngOnDestroy() {
        this.addBodyScroll();
        subscriptionHandler(this.$subscriptions);
    }

    getEditMode() {
        const DATA = this._dataStore.getData(this.dependencies);
        this.conflictStatusCode = 0;
        this.conflictStatusCode = DATA.coiDisclosure.conflictStatusCode;
        this.reviewStatus = DATA.coiDisclosure.reviewStatusCode;
        this.disclosureId =  DATA.coiDisclosure.disclosureId;
        this.isEditMode = this._dataStore.getEditModeForCOI();
        this.personId = DATA.coiDisclosure.personId;
        this.isSFINotAvailable = DATA.numberOfSFI === 0 && DATA.coiDisclosure.disclosureCategoryTypeCode == 3;
    }

    getSfiDetails() {
        this.$subscriptions.push(this.$fetchSFIList.pipe(
            switchMap(() => this._sfiService.getSfiDetails(this.getRequestObject()))).subscribe((data: any) => {
            if (data) {
                this.count = data.count;
                this.coiFinancialEntityDetails = data.personEntities;
                this.isLoading = false;
            }
        }));
    }

    getRequestObject() {
        let requestObj: GetSFIRequestObject = new GetSFIRequestObject();
        requestObj.currentPage = this.currentPage;
        requestObj.disclosureId = this.disclosureId;
        requestObj.filterType = this.filterType;
        requestObj.pageNumber = '10';
        requestObj.personId = this.personId;
        requestObj.reviewStatusCode = this.reviewStatus;
        requestObj.searchWord = this.searchText;
        return requestObj;
    }

    listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                if (dependencies.some((dep) => this.dependencies.includes(dep))) {
                    this.getEditMode();
                }
            })
        );
    }

    listenForAdd() {
        this.$subscriptions.push(
            this._sfiService.$addSfi.subscribe((data: boolean) => {
                this.$fetchSFIList.next();
                this._sfiService.isShowSfiNavBar = false;
                this.removeEntityId();
            })
        );
    }

    viewSlider(event) {
        this.showSlider = event.flag;
        this.entityId = event.entityId;
        document.getElementById('COI_SCROLL').classList.add('overflow-hidden');
        setTimeout(() => {
            const slider = document.querySelector('.slider-base');
            slider.classList.add('slider-opened');
        });
    }

    setFilter(filterType) {
        this.filterType = filterType;
        this.currentPage = 1;
        this.searchText = '';
        this.isLoading = true;
        this.coiFinancialEntityDetails = [];
        this.$fetchSFIList.next();
    }

    removeEntityId() {
        this._router.navigate([], {
          queryParams: {entityId: null},
          queryParamsHandling: 'merge'
        })
      }

    closeSFIInfo() {
        this._coiService.isShowSFIInfo = false;
    }

    actionsOnPageChange(event) {
            this.currentPage = event;
            this.$fetchSFIList.next();
    }

    hideSfiNavBar() {
        this.addBodyScroll();
        let slider = document.querySelector('.slider-base');
        slider.classList.remove('slider-opened');        
        setTimeout(() => {
            this.showSlider = false;
        },500);
    }

    addBodyScroll() {
        document.getElementById('COI_SCROLL').classList.remove('overflow-hidden');
        document.getElementById('COI_SCROLL').classList.add('overflow-y-scroll');
    }

    deleteSFIConfirmation(event, i) {
        this.personEntityId = event.eId;
        this.entityName = this.coiFinancialEntityDetails.find(ele => ele.personEntityId === this.personEntityId).coiEntity.entityName;
        document.getElementById('hidden-delete-button').click();
    }

    deleteSFI() {
        this._sfiService.deleteSFI(this.personEntityId).subscribe((data:any) => {
            this.currentPage = 1;
            this.$fetchSFIList.next();
            this._commonService.showToast(HTTP_SUCCESS_STATUS, 'SFI deleted successfully.');
        }, err=> {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'SFI deletion canceled.');
        })
      }

      activateDeactivateEvent(event) {
        this.isEnableActivateInactivateSfiModal = true;
        this.personEntityId = event.personEntityId;
        this.entityName = event.coiEntity.entityName;
        this.isRelationshipActive = event.isRelationshipActive;
      }

      closeActivateInactivateSfiModal(event) {
          this.isEnableActivateInactivateSfiModal = false;
          if(event) {
            this.$fetchSFIList.next();
        }
      }

      getEntities() {
        this.currentPage = 1;
        this.$debounceEvent.next('');
      }
    
      getSearchList() {
        this.$subscriptions.push(this.$debounceEvent.pipe(debounce(() => interval(1000))).subscribe((data: any) => {
          this.$fetchSFIList.next();
        }
        ));
      }

      clearSearchText() {
        this.searchText = '';
        this.$fetchSFIList.next(); 
      }
}
