import {Component, ElementRef, OnDestroy, OnInit, ViewChild} from '@angular/core';
import { closeCoiSlider, openCoiSlider } from '../../common/utilities/custom-utilities';
import { Router } from '@angular/router';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { EntityDataStoreService } from '../entity-data-store.service';
import { EntireEntityDetails, EntityDetails, EntityTabStatus, EntityDetailsCard } from '../shared/entity-interface';
import { AutoSaveService } from '../../common/services/auto-save.service';
import {subscriptionHandler} from "../../../../../fibi/src/app/common/utilities/subscription-handler";
import { EntityManagementService } from '../entity-management.service';

class DNBReqObj {
    sourceDataName: string;
    sourceDunsNumber: any;
    emailAddress: string;
    addressLine1: string;
    addressLine2: string;
    postalCode: string;
    state: string;
    countryCode: string;
}import { ModalActionEvent } from '../../shared-components/coi-modal/coi-modal.interface';

@Component({
  selector: 'app-header-details',
  templateUrl: './header-details.component.html',
  styleUrls: ['./header-details.component.scss']
})
export class HeaderDetailsComponent implements OnInit, OnDestroy {

    @ViewChild('mainEntityHeaders', { static: true }) mainEntityHeaders: ElementRef;

    sliderElementId: any;
    isShowOptions = false;
    showSlider = false;
    isShowNavBarOverlay = false;
    $subscriptions: Subscription[] = [];
    entityDetails: EntityDetails = new EntityDetails();
    entityFullAddress: string = '';
    latestPriorName: any;
    isEditMode = false;
    matchedEntites: any;
    isOpenVerifyModal = false;
    entityTabStatus = new EntityTabStatus();

    constructor(public router: Router, public dataStore: EntityDataStoreService,
        public autoSaveService: AutoSaveService, private _entityManagementService: EntityManagementService) {}
    ngOnInit() {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    viewSlider(event) {
        this.$subscriptions.push(this._entityManagementService.getDunsMatch(this.getReqObj()).subscribe((data: any) => {
        this.matchedEntites = data?.matchCandidates?.length ? data?.matchCandidates : [];
        if(this.matchedEntites.length) {
            this.matchedEntites.map(ele =>this.formatResponse(ele));
        }
        this.showSlider = event;
        this.sliderElementId = 'duns-match-slider';
        setTimeout(() => {
            openCoiSlider(this.sliderElementId);
        });
    }))
    }

    getReqObj(): any {
        let reqObj = new DNBReqObj();
        reqObj.sourceDataName = this.entityDetails.entityName;
        reqObj.sourceDunsNumber = this.entityDetails.dunsNumber || '';
        reqObj.addressLine1 = this.entityDetails.primaryAddressLine1;
        reqObj.addressLine2 = this.entityDetails.primaryAddressLine2 || '';
        reqObj.countryCode = this.entityDetails?.country?.countryTwoCode;
        reqObj.state = '';
        reqObj.postalCode = this.entityDetails.postCode || '';
        reqObj.emailAddress = this.entityDetails.certifiedEmail || '';
        return reqObj;
    }

    validateSliderClose() {
        setTimeout(() => {
            this.showSlider = false;
            this.sliderElementId = '';
		}, 500);
	  }

    onClickMenuBar() {
        const NAV_ELEMENT = document.getElementById('responsive-nav');
        const IS_MENU_SHOW = NAV_ELEMENT.classList.contains('show-menu');
        const IS_SCREEN = window.innerWidth <= 1300;

        if (IS_MENU_SHOW) {
            NAV_ELEMENT.classList.remove('show-menu');
            if (IS_SCREEN) {
                this.isShowNavBarOverlay = false;
            }
        } else {
            if (IS_SCREEN) {
                this.isShowNavBarOverlay = true;
            }
            NAV_ELEMENT.classList.toggle('show-menu', IS_SCREEN);
        }
    }

    private getDataFromStore() {
        const ENTITY_DATA: EntireEntityDetails = this.dataStore.getData();
        if (!ENTITY_DATA || isEmptyObject(ENTITY_DATA)) { return; }
        this.entityDetails = ENTITY_DATA.entityDetails;
        this.latestPriorName = ENTITY_DATA?.priorNames?.[0]?.priorNames;
        this.entityTabStatus = ENTITY_DATA.entityTabStatus
        this.getEntityFullAddress();
        this.isEditMode = this.dataStore.getEditMode();
    }

    getEntityFullAddress() {
        let address = this.entityDetails?.primaryAddressLine1;
        if (this.entityDetails?.primaryAddressLine2) {
            address = address + ' , ' + this.entityDetails?.primaryAddressLine2;
        }
        if(this.entityDetails?.city) {
            address = address + ' , ' + this.entityDetails?.city;
        }
        if(this.entityDetails?.state) {
            address = address + ' , ' + this.entityDetails?.state;
        }
        if(this.entityDetails?.country?.countryName) {
            address = address + ' , ' + this.entityDetails?.country?.countryName;
        }
        this.entityFullAddress = address;
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this.dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    formatResponse(entity) {
        entity.organization.entityName = entity.organization.primaryName;
        entity.organization.state = entity.organization?.primaryAddress?.addressRegion?.abbreviatedName;
        entity.organization.DUNSNumber = entity.organization.duns;
        entity.organization.entityAddress = entity.organization?.primaryAddress?.streetAddress?.line1 +
            (entity.organization?.primaryAddress?.streetAddress?.line2 ? ','+ entity.organization?.primaryAddress?.streetAddress?.line2 : '' );
        entity.organization.city = entity.organization?.primaryAddress?.addressLocality?.name;
        entity.organization.country = entity.organization?.primaryAddress?.addressCountry?.name;
        entity.organization.phoneNumber = entity?.organization?.telephone[0]?.telephoneNumber;
        entity.organization.zipCode = entity.organization?.primaryAddress?.postalCode;
    }

    updateEntireEntity(isDunsMatched, entity) {
        this._entityManagementService.triggerDUNSEntity.next({orgDetails : entity?.organization, isDunsMatched: isDunsMatched});
        closeCoiSlider('duns-match-slider');
        this.validateSliderClose();
    }

    openVerifyEntityModal(): void {
        this.isOpenVerifyModal = true;
    }

    verifyModalAction(modalAction: ModalActionEvent | null): void {
        this.isOpenVerifyModal = false;
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    navigateToBack() {
        this.router.navigate(['/coi/entity-dashboard'])
    }
}
