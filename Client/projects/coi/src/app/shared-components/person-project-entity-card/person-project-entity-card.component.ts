import { Component, Input } from '@angular/core';
import { PersonProjectOrEntity } from '../shared-interface';

@Component({
  selector: 'app-person-project-entity-card',
  templateUrl: './person-project-entity-card.component.html',
  styleUrls: ['./person-project-entity-card.component.scss']
})

export class PersonProjectEntityCardComponent {

  @Input() personProjectOrEntity: PersonProjectOrEntity = new PersonProjectOrEntity();

  isReadMore = false;

}
