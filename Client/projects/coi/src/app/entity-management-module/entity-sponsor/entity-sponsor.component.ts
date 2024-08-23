import { Component } from '@angular/core';
import { CommonService } from '../../common/services/common.service';
import { EntityDataStoreService } from '../entity-data-store.service';
import { SponsorTabSection } from '../shared/entity-constants';

@Component({
    selector: 'app-entity-sponsor',
    templateUrl: './entity-sponsor.component.html',
    styleUrls: ['./entity-sponsor.component.scss']
})
export class EntitySponsorComponent {

    overViewTab: any;

    constructor(public commonService: CommonService, public dataStore: EntityDataStoreService) { }


    ngOnInit() {
        window.scrollTo(0,0);
        this.overViewTab = SponsorTabSection;
    }

}
