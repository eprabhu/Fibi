import { Component } from '@angular/core';
import { EntityDetailsCard } from '../entity-interface';

@Component({
  selector: 'app-entity-common-card',
  templateUrl: './entity-common-card.component.html',
  styleUrls: ['./entity-common-card.component.scss']
})
export class EntityCommonCardComponent {

    isExpanded = false;
    entityDetailsObj: EntityDetailsCard = new EntityDetailsCard();

}
