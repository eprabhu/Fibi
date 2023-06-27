import { Component, ElementRef, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { Subscription } from 'rxjs';

import { SfiService } from './sfi.service';
import { DataStoreService } from '../services/data-store.service';
import { CoiService } from '../services/coi.service';
import {subscriptionHandler} from "../../../../../fibi/src/app/common/utilities/subscription-handler";
import { NavigationEnd, Router } from '@angular/router';
import { CommonService } from '../../common/services/common.service';
import { slideHorizontalFast } from '../../../../../fibi/src/app/common/utilities/animations';
import { HTTP_SUCCESS_STATUS, HTTP_ERROR_STATUS } from '../../app-constants';

@Component({
    selector: 'app-sfi',
    templateUrl: './sfi.component.html',
    styleUrls: ['./sfi.component.scss'],
    animations: [slideHorizontalFast]
})
export class SfiComponent implements OnInit, OnDestroy {
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

    @ViewChild('sfiNewOverlay', { static: true }) sfiNewOverlay: ElementRef;
    isEnableActivateInactivateSfiModal: boolean;
    
    constructor(
        private _sfiService: SfiService,
        private _dataStore: DataStoreService,
        public _coiService: CoiService,
        private _router: Router,
        private _commonService: CommonService) {
    }

    ngOnInit() {
        this._coiService.isShowSFIInfo = true;
        this.getEditMode();
        this.getSfiDetails();
        this.listenDataChangeFromStore();
        this.listenForAdd();
    }

    ngOnDestroy() {
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
        this.$subscriptions.push(this._sfiService.getSfiDetails(this.disclosureId, this.reviewStatus, this.personId, this.filterType, this.currentPage).subscribe((data: any) => {
            if (data) {
                this.count = data.count;
                this.coiFinancialEntityDetails = data.personEntities;
            }
        }));
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
                this.getSfiDetails();
                this._sfiService.isShowSfiNavBar = false;
                this.removeEntityId();
            })
        );
    }

    viewSlider(event) {
        this.showSlider = event.flag;
        this.entityId = event.entityId;
        this.showSfiNavBar();
    }

    setFilter(filterType) {
        this.filterType = filterType;
        this.currentPage = 1;
        this.searchText = '';
        this.getSfiDetails();
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
        if(this.currentPage != event) {
            this.currentPage = event;
            this.searchText = '';
            this.getSfiDetails();
        }
    }

    hideSfiNavBar() {
        this.showSlider = false;
        this.showSfiNavBar();
    }

    showSfiNavBar() {
        if (this.showSlider) {
            this.scrollHeight = document.documentElement.scrollTop;
            document.body.style.overflow = 'hidden';
            document.documentElement.style.top = - this.scrollHeight + 'px';
        } else {
            document.body.style.overflow = 'auto';
            document.documentElement.scrollTop = this.scrollHeight;
        }
    }

    deleteSFIConfirmation(event, i) {
        this.personEntityId = event.eId;
        this.entityName = this.coiFinancialEntityDetails.find(ele => ele.personEntityId === this.personEntityId).coiEntity.entityName;
        document.getElementById('hidden-delete-button').click();
    }

    deleteSFI() {
        this._sfiService.deleteSFI(this.personEntityId).subscribe((data:any) => {
            this.currentPage = 1;
            this.getSfiDetails();
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
            this.getSfiDetails();
          }
      }
}
