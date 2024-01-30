import { Component, ElementRef, HostListener, Input, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { Subject, Subscription, interval } from 'rxjs';

import { SfiService } from './sfi.service';
import {subscriptionHandler} from "../../../../../fibi/src/app/common/utilities/subscription-handler";
import { ActivatedRoute, Router } from '@angular/router';
import { CommonService } from '../../common/services/common.service';
import { HTTP_SUCCESS_STATUS, HTTP_ERROR_STATUS } from '../../app-constants';
import { debounce, switchMap } from 'rxjs/operators';
import { RO } from '../coi-interface';
import { fadeInOutHeight, leftSlideInOut, listAnimation } from '../../common/utilities/animations';
import { scrollIntoView } from '../../../../../fibi/src/app/common/utilities/custom-utilities';

@Component({
    selector: 'app-sfi',
    templateUrl: './sfi.component.html',
    styleUrls: ['./sfi.component.scss'],
    animations: [listAnimation, fadeInOutHeight, leftSlideInOut]
})
export class SfiComponent implements OnInit, OnDestroy {

    @ViewChild('viewSFIDetailsOverlay', { static: true }) viewSFIDetailsOverlay: ElementRef;
    @Input()  isTriggeredFromSlider = false;
    @Input()  reviewStatus: any;
    @Input()  isEditMode: any;
    @Input()  personId: any;
    @Input()  focusSFIId: any;
    $subscriptions: Subscription[] = [];
    coiFinancialEntityDetails: any[] = [];
    searchText: string;
    searchResult = [];
    disclosureId: any;
    dependencies = ['coiDisclosure', 'numberOfSFI'];
    filterType = 'ALL';
    currentPage = 1;
    count: any;
    showSlider = false;
    scrollHeight: number;
    entityId: any;
    personEntityId: any;
    entityName: any;
    updatedRelationshipStatus: string;
    entityDetails: any;
    expandInfo = false;
    isEnableActivateInactivateSfiModal: boolean;
    $debounceEvent = new Subject();
    $fetchSFIList = new Subject();
    isSearchTextHover = false;
    isLoading = false;
    personEntityNumber: any;

    constructor(
        private _sfiService: SfiService,
        private _router: Router,
        private _activatedRoute: ActivatedRoute,
        private _commonService: CommonService,
        private elementRef: ElementRef) {
    }

    ngOnInit() {
        this.getSfiDetails();
        this.$fetchSFIList.next();
        this.getSearchList();
        this.listenForAdd();
    }

    ngOnDestroy() {
        this.addBodyScroll();
        subscriptionHandler(this.$subscriptions);
    }

    getSfiDetails() {
        this.$subscriptions.push(this.$fetchSFIList.pipe(
            switchMap(() => {
                this.isLoading = true;
                return this._sfiService.getSfiDetails(this.getRequestObject())
            })).subscribe((data: any) => {
            if (data) {
                this.count = data.count;
                this.coiFinancialEntityDetails = data.personEntities;
                this.isLoading = false;
                setTimeout(() => {
                    if(this.focusSFIId) {
                        scrollIntoView(this.focusSFIId);
                        const ELEMENT = document.getElementById(this.focusSFIId);
                        ELEMENT.classList.add('error-highlight-card');
                        this.focusSFIId = null;
                    }
            });
            }
        }));
    }

    getRequestObject() {
        let requestObj: RO = new RO();
        requestObj.currentPage = this.currentPage;
        requestObj.disclosureId = !this.isTriggeredFromSlider ? this._activatedRoute.snapshot.queryParamMap.get('disclosureId') : null;
        requestObj.filterType = this.filterType;
        requestObj.pageNumber = '10';
        requestObj.personId = this.personId;
        requestObj.reviewStatusCode = this.reviewStatus;
        requestObj.searchWord = this.searchText;
        return requestObj;
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
        this.coiFinancialEntityDetails = [];
        this.$fetchSFIList.next();
    }

    removeEntityId() {
        this._router.navigate([], {
          queryParams: {entityId: null},
          queryParamsHandling: 'merge'
        })
      }

    actionsOnPageChange(event) {
        if (this.currentPage != event) {
            this.currentPage = event;
            this.$fetchSFIList.next();
        }
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
        this.updatedRelationshipStatus = event.versionStatus === 'ACTIVE' ? 'INACTIVE' : 'ACTIVE';
        this.personEntityNumber = event.personEntityNumber;
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

      onWindowScroll(event) {
          const pageYOffset = this.elementRef.nativeElement.querySelector('.slider-container').scrollTop;
          this._commonService.$sliderScrollAction.next({event, pageYOffset});
      }
}
