
import { TestBed, inject } from '@angular/core/testing';
import { QuestionnaireService } from './questionnaire.service';

describe('Service: Questionnaire', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [QuestionnaireService]
    });
  });

  it('should ...', inject([QuestionnaireService], (service: QuestionnaireService) => {
    expect(service).toBeTruthy();
  }));
});
