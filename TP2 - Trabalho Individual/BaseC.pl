
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - Exercicio 2 Individual

% Tania Rocha

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais


:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic '-'/1.
:- dynamic(paragem/11).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extenção do predicado paragem: Id, latitude, longitude, Estado de Conservacao
%								, Tipo de Abrigo, Abrigo com Publicidade?, Operadora
%								, Carreira, Codigo de Rua, Nome de Rua, Freguesia -> {V,F,D}

%Base de Conhecimento

paragem(79,-107011.55,-95214.57,'Bom','Fechado dos Lados','Yes','Vimeca',[01],103,'Rua Damiao de Gois','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(593,-103777.02,-94637.67,'Bom','Sem Abrigo','No','Vimeca',[01],300,'Avenida dos Cavaleiros','Carnaxide e Queijas').
paragem(499,-103758.44,-94393.36,'Bom','Fechado dos Lados','Yes','Vimeca',[01],300,'Avenida dos Cavaleiros','Carnaxide e Queijas').
paragem(494,-106803.2,-96265.84,'Bom','Sem Abrigo','No','Vimeca',[01],389,'Rua Sao Joao de Deus','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(480,-106757.3,-96240.22,'Bom','Sem Abrigo','No','Vimeca',[01],389,'Rua Sao Joao de Deus','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(957,-106911.18264993647,-96261.15727273725,'Bom','Sem Abrigo','No','Vimeca',[01],399,'Escadinhas da Fonte da Maruja','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(366,-106021.37,-96684.5,'Bom','Fechado dos Lados','Yes','Vimeca',[01],411,'Avenida Dom Pedro V','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(365,-106016.12,-96673.87,'Bom','Fechado dos Lados','Yes','Vimeca',[01],411,'Avenida Dom Pedro V','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(357,-105236.99,-96664.4,'Bom','Fechado dos Lados','Yes','Vimeca',[01],1279,'Avenida Tomas Ribeiro','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(336,-105143.57,-96690.32,'Bom','Fechado dos Lados','Yes','Vimeca',[01],1279,'Avenida Tomas Ribeiro','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(334,-105336.07,-96668.68,'Bom','Fechado dos Lados','Yes','Vimeca',[01],1279,'Avenida Tomas Ribeiro','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(251,-104487.69,-96548.01,'Bom','Fechado dos Lados','Yes','Vimeca',[01],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(469,-106613.44,-96288,'Bom','Fechado dos Lados','Yes','Vimeca',[01],1288,'Rua Rodrigo Albuquerque e Melo','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(462,-106636.23,-96302.04,'Bom','Sem Abrigo','No','Vimeca',[01],1288,'Rua Rodrigo Albuquerque e Melo','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(44,-104458.52,-94926.22,'Bom','Fechado dos Lados','Yes','Vimeca',[01,13,15],1134,'Largo Sete de Junho de 1759','Carnaxide e Queijas').
paragem(78,-107008.56,-95490.23,'Bom','Fechado dos Lados','Yes','Vimeca',[01,02,06,14],118,'Alameda Hermano Patrone','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(609,-104226.49,-95797.22,'Bom','Fechado dos Lados','Yes','Vimeca',[01,02,07,10,12,13,15],327,'Avenida do Forte','Carnaxide e Queijas').
paragem(599,-104296.72,-95828.26,'Bom','Fechado dos Lados','Yes','Vimeca',[01,02,07,10,12,13,15],327,'Avenida do Forte','Carnaxide e Queijas').
paragem(595,-103725.69,-95975.2,'Bom','Fechado dos Lados','Yes','Vimeca',[01,02,07,10,12,13,15],354,'Rua Manuel Teixeira Gomes','Carnaxide e Queijas').
paragem(185,-103922.82,-96235.62,'Bom','Fechado dos Lados','Yes','SCoTTURB',[01,02,07,10,12,13,15],354,'Rua Manuel Teixeira Gomes','Carnaxide e Queijas').
paragem(250,-104031.08,-96173.83,'Bom','Fechado dos Lados','Yes','Vimeca',[01,02,07,10,12,13,15],1113,'Avenida de Portugal','Carnaxide e Queijas').
paragem(107,-103972.32,-95981.88,'Bom','Fechado dos Lados','Yes','Vimeca',[01,02,07,10,12,13,15],1113,'Avenida de Portugal','Carnaxide e Queijas').
paragem(953,-104075.89,-95771.82,'Bom','Fechado dos Lados','Yes','Vimeca',[01,02,07,10,12,13,15],1116,'Avenida Professor Dr. Reinaldo dos Santos','Carnaxide e Queijas').
paragem(594,-103879.91,-95751.23,'Bom','Fechado dos Lados','No','Vimeca',[01,02,07,10,12,13,15],1116,'Avenida Professor Dr. Reinaldo dos Santos','Carnaxide e Queijas').
paragem(597,-104058.98,-95839.14,'Bom','Fechado dos Lados','Yes','Vimeca',[01,02,07,10,12,13,15],1137,'Rua Tenente-General Zeferino Sequeira','Carnaxide e Queijas').
paragem(261,-104032.88,-96536.98,'Bom','Fechado dos Lados','Yes','Vimeca',[01,02,10],1113,'Avenida de Portugal','Carnaxide e Queijas').
paragem(341,-105797.42,-96746.57,'Bom','Fechado dos Lados','Yes','Vimeca',[01,02,11],411,'Avenida Dom Pedro V','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(85,-105653.28,-96814.42,'Bom','Fechado dos Lados','Yes','Vimeca',[01,02,11],411,'Avenida Dom Pedro V','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(347,-105493.13,-96785.72,'Bom','Fechado dos Lados','Yes','Vimeca',[01,02,11],432,'Calcada do Chafariz','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(342,-105815.99,-96725.14,'Bom','Fechado dos Lados','Yes','Vimeca',[01,02,11,13],411,'Avenida Dom Pedro V','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(86,-105637.56,-96808.45,'Bom','Fechado dos Lados','Yes','Vimeca',[01,02,11,13],411,'Avenida Dom Pedro V','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(339,-105452.81,-96732.86,'Bom','Fechado dos Lados','Yes','Vimeca',[01,02,11,13],432,'Calcada do Chafariz','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(186,-106975.22,-95602.61,'Bom','Fechado dos Lados','No','Vimeca',[01,06],118,'Alameda Hermano Patrone','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(467,-107004.52,-96080.98,'Bom','Fechado dos Lados','No','Vimeca',[01,06],369,'Rua Direita do Dafundo','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(466,-106999.08,-96066.1,'Bom','Fechado dos Lados','No','Vimeca',[01,06],369,'Rua Direita do Dafundo','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(465,-106915.82,-96269.57,'Bom','Sem Abrigo','No','Vimeca',[01,06],369,'Rua Direita do Dafundo','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(791,-103705.46,-96673.6,'Bom','Aberto dos Lados','Yes','Vimeca',[01,07,10,12,13,15],286,'Rua Aquilino Ribeiro','Carnaxide e Queijas').
paragem(183,-103678.36,-96590.26,'Bom','Fechado dos Lados','Yes','Vimeca',[01,07,10,12,13,15],286,'Rua Aquilino Ribeiro','Carnaxide e Queijas').
paragem(182,-103746.76,-96396.66,'Bom','Fechado dos Lados','Yes','SCoTTURB',[01,07,10,12,13,15],286,'Rua Aquilino Ribeiro','Carnaxide e Queijas').
paragem(181,-103780.59,-96372.2,'Bom','Aberto dos Lados','Yes','Vimeca',[01,07,10,12,13,15],286,'Rua Aquilino Ribeiro','Carnaxide e Queijas').
paragem(180,-103842.39,-96260.96,'Bom','Fechado dos Lados','Yes','Vimeca',[01,07,10,12,13,15],286,'Rua Aquilino Ribeiro','Carnaxide e Queijas').
paragem(89,-103934.24,-96642.56,'Bom','Fechado dos Lados','Yes','Vimeca',[01,07,10,12,13,15],1113,'Avenida de Portugal','Carnaxide e Queijas').
paragem(604,-104256.82,-95173.34,'Bom','Fechado dos Lados','No','Vimeca',[01,10,13,15],306,'Rua dos Cravos de Abril','Carnaxide e Queijas').
paragem(40,-104302.13,-95043.86,'Bom','Fechado dos Lados','Yes','Vimeca',[01,10,13,15],306,'Rua dos Cravos de Abril','Carnaxide e Queijas').
paragem(39,-104282.32,-95055.6,'Bom','Fechado dos Lados','Yes','Vimeca',[01,10,13,15],306,'Rua dos Cravos de Abril','Carnaxide e Queijas').
paragem(620,-104565.8832899218,-94653.67859291832,'Bom','Sem Abrigo','No','Vimeca',[01,10,13,15],365,'Estrada da Portela','Carnaxide e Queijas').
paragem(45,-104578.88,-94652.12,'Bom','Sem Abrigo','No','Vimeca',[01,10,13,15],365,'Estrada da Portela','Carnaxide e Queijas').
paragem(51,-104458.04,-94329.86,'Bom','Fechado dos Lados','No','Vimeca',[01,10,13,15],1123,'Rua da Quinta do Paizinho','Carnaxide e Queijas').
paragem(628,-104278.88666597521,-94122.56603635015,'Bom','Sem Abrigo','No','Vimeca',[01,10,13,15],1123,'Rua da Quinta do Paizinho','Carnaxide e Queijas').
paragem(50,-104287.85,-94105.37,'Bom','Fechado dos Lados','Yes','Vimeca',[01,10,13,15],1123,'Rua da Quinta do Paizinho','Carnaxide e Queijas').
paragem(38,-104497.842173306,-94358.908881103,'Bom','Fechado dos Lados','Yes','Vimeca',[01,10,13,15],1123,'Rua da Quinta do Paizinho','Carnaxide e Queijas').
paragem(622,-104445.64,-94921.33,'Bom','Fechado dos Lados','No','Vimeca',[01,10,13,15],1134,'Largo Sete de Junho de 1759','Carnaxide e Queijas').
paragem(602,-104677.06,-94473.47,'Bom','Fechado dos Lados','No','Vimeca',[01,10,13,15],1160,'Rua Cincinato da Costa','Carnaxide e Queijas').
paragem(601,-104683.1,-94486.15,'Bom','Fechado dos Lados','No','Vimeca',[01,10,13,15],1160,'Rua Cincinato da Costa','Carnaxide e Queijas').
paragem(485,-106315.88,-96307.18,'Bom','Fechado dos Lados','Yes','Vimeca',[01,11],1289,'Rua Castro Soromenho','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(488,-106492.31,-96447.01,'Bom','Sem Abrigo','No','Vimeca',[01,11],1292,'Rua Manuel Ferreira','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(486,-106325.58,-96320.92,'Bom','Fechado dos Lados','Yes','Vimeca',[01,11,13],1289,'Rua Castro Soromenho','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(487,-106449.51,-96435.13,'Bom','Sem Abrigo','No','Vimeca',[01,11,13],1292,'Rua Manuel Ferreira','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(468,-106270.67,-96457.19,'Bom','Fechado dos Lados','Yes','Vimeca',[01,11,13],1292,'Rua Manuel Ferreira','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(460,-106247.39,-96517.97,'Bom','Fechado dos Lados','Yes','Vimeca',[01,13],1292,'Rua Manuel Ferreira','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(249,-104397.14,-96680.46,'Bom','Fechado dos Lados','Yes','Vimeca',[01,13,15],297,'Rua Carlos Wallenstein','Carnaxide e Queijas').
paragem(600,-104631.8,-95630.5,'Bom','Fechado dos Lados','No','Vimeca',[01,13,15],351,'Rua Manuel Antonio Rodrigues','Carnaxide e Queijas').
paragem(42,-104624.97,-95613.11,'Bom','Sem Abrigo','No','Vimeca',[01,13,15],351,'Rua Manuel Antonio Rodrigues','Carnaxide e Queijas').
paragem(614,-104594.97,-95684.18,'Bom','Fechado dos Lados','No','Vimeca',[01,13,15],359,'Rua Nossa Senhora da Conceicao','Carnaxide e Queijas').
paragem(46,-104609.99,-95693.01,'Bom','Fechado dos Lados','Yes','Vimeca',[01,13,15],359,'Rua Nossa Senhora da Conceicao','Carnaxide e Queijas').
paragem(611,-104989.68,-95554.55,'Bom','Fechado dos Lados','No','Vimeca',[01,13,15],1196,'Rua Carlos Belo Morais','Carnaxide e Queijas').
paragem(610,-104998.77,-95557.54,'Bom','Sem Abrigo','No','Vimeca',[01,13,15],1196,'Rua Carlos Belo Morais','Carnaxide e Queijas').
paragem(49,-104758.56,-95206.97,'Bom','Fechado dos Lados','Yes','Vimeca',[01,13,15],1196,'Rua Carlos Belo Morais','Carnaxide e Queijas').
paragem(48,-104710.71,-95177.32,'Bom','Fechado dos Lados','No','Vimeca',[01,13,15],1196,'Rua Carlos Belo Morais','Carnaxide e Queijas').
paragem(613,-104817.75,-95640.29,'Bom','Fechado dos Lados','No','Vimeca',[01,13,15],1197,'Rua Mario Moreira','Carnaxide e Queijas').
paragem(612,-104807.71,-95652.96,'Bom','Sem Abrigo','No','Vimeca',[01,13,15],1197,'Rua Mario Moreira','Carnaxide e Queijas').
paragem(985,-104367.95010080478,-95373.18330437147,'Bom','Sem Abrigo','No','Vimeca',[01,13,15],1237,'Avenida Professor Dr. Bernardino Machado','Carnaxide e Queijas').
paragem(608,-104373.51,-95357.73,'Bom','Fechado dos Lados','Yes','Vimeca',[01,13,15],1237,'Avenida Professor Dr. Bernardino Machado','Carnaxide e Queijas').
paragem(255,-104240.6,-96543.14,'Bom','Fechado dos Lados','Yes','Vimeca',[01,13,15],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(254,-104407,-96522.21,'Bom','Fechado dos Lados','Yes','Vimeca',[01,13,15],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(242,-104235.94,-96573.14,'Bom','Fechado dos Lados','Yes','Vimeca',[01,13,15],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(80,-107020.11,-95212.99,'Bom','Fechado dos Lados','Yes','Vimeca',[02],103,'Rua Damiao de Gois','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(655,-106885.28346821875,-95700.604683315,'Bom','Sem Abrigo','No','Vimeca',[02],121,'Rua Joao Chagas','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(491,-106752.7,-95980.67,'Bom','Fechado dos Lados','Yes','Vimeca',[02],121,'Rua Joao Chagas','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(490,-106724.64,-96023.19,'Bom','Fechado dos Lados','Yes','Vimeca',[02],121,'Rua Joao Chagas','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(458,-106344.84,-96171.5,'Bom','Fechado dos Lados','Yes','Vimeca',[02],121,'Rua Joao Chagas','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(457,-106251.29,-96169.58,'Bom','Fechado dos Lados','Yes','Vimeca',[02],121,'Rua Joao Chagas','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(56,-106877.17,-95698.23,'Bom','Sem Abrigo','No','Vimeca',[02],121,'Rua Joao Chagas','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(343,-105860.11,-96563.44,'Bom','Fechado dos Lados','Yes','Vimeca',[02],457,'Rua Francisco Jose Victorino','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(237,-104003.78,-96559.17,'Bom','Fechado dos Lados','Yes','Vimeca',[02],1113,'Avenida de Portugal','Carnaxide e Queijas').
paragem(245,-104114.84,-97401.12,'Bom','Fechado dos Lados','Yes','Vimeca',[02],1214,'Estrada de Queluz','Carnaxide e Queijas').
paragem(244,-104101.68,-97408.6,'Bom','Fechado dos Lados','Yes','Vimeca',[02],1214,'Estrada de Queluz','Carnaxide e Queijas').
paragem(734,-102676.09,-98810.3,'Bom','Sem Abrigo','No','Vimeca',[02,02,12,13],950,'Estrada das Palmeiras','Barcarena').
paragem(745,-102136.13485160771,-98663.30880207638,'Bom','Fechado dos Lados','No','Vimeca',[02,06,12,13],216,'Estrada Consiglieri Pedroso','Barcarena').
paragem(736,-102231.41,-98789.31,'Bom','Fechado dos Lados','Yes','Vimeca',[02,06,12,13],216,'Estrada Consiglieri Pedroso','Barcarena').
paragem(147,-102381.73,-98965.83,'Bom','Sem Abrigo','No','Vimeca',[02,06,12,13],950,'Estrada das Palmeiras','Barcarena').
paragem(227,-104412.8,-98632.87,'Bom','Sem Abrigo','No','Vimeca',[02,06,13],805,'Rua Ilha de Sao Jorge','Carnaxide e Queijas').
paragem(172,-103411.08,-99046.23,'Bom','Sem Abrigo','No','SCoTTURB',[02,06,13],830,'Estrada Militar','Barcarena').
paragem(171,-103417.17,-99041.11,'Bom','Sem Abrigo','No','Vimeca',[02,06,13],830,'Estrada Militar','Barcarena').
paragem(162,-102962.16,-98672.14,'Bom','Sem Abrigo','No','Vimeca',[02,06,13],830,'Estrada Militar','Barcarena').
paragem(161,-102932.36,-98676.69,'Bom','Fechado dos Lados','Yes','Vimeca',[02,06,13],830,'Estrada Militar','Barcarena').
paragem(156,-102400.99,-98945.23,'Bom','Sem Abrigo','No','Vimeca',[02,06,13],950,'Estrada das Palmeiras','Barcarena').
paragem(1010,-104303.63612383853,-98554.77838335252,'Bom','Sem Abrigo','No','Vimeca',[02,06,13,15],79,'Rua dos Acores','Carnaxide e Queijas').
paragem(224,-104563.77,-98320.53,'Bom','Sem Abrigo','No','Vimeca',[02,06,13,15],833,'Rua Mouzinho da Silveira','Carnaxide e Queijas').
paragem(234,-104471.99,-98565.73,'Bom','Fechado dos Lados','No','Vimeca',[02,06,13,15],83,'Rua Angra do Heroismo','Carnaxide e Queijas').
paragem(233,-104935.73,-98290.43,'Bom','Fechado dos Lados','Yes','Vimeca',[02,06,13,15],813,'Rua Joao XXI','Carnaxide e Queijas').
paragem(232,-104768.69,-98266.88,'Bom','Fechado dos Lados','No','SCoTTURB',[02,06,13,15],813,'Rua Joao XXI','Carnaxide e Queijas').
paragem(231,-104942.78,-98303.15,'Bom','Fechado dos Lados','Yes','Vimeca',[02,06,13,15],813,'Rua Joao XXI','Carnaxide e Queijas').
paragem(52,-104801.2,-98279.24,'Bom','Fechado dos Lados','Yes','Vimeca',[02,06,13,15],813,'Rua Joao XXI','Carnaxide e Queijas').
paragem(230,-104447.68,-98306.88,'Bom','Sem Abrigo','No','Vimeca',[02,06,13,15],833,'Rua Mouzinho da Silveira','Carnaxide e Queijas').
paragem(226,-104618.82,-98507.86,'Bom','Fechado dos Lados','No','Vimeca',[02,06,13,15],846,'Rua da Quinta do Bonfim','Carnaxide e Queijas').
paragem(799,-104280.83,-98312.61,'Bom','Fechado dos Lados','No','Vimeca',[02,06,13,15],1766,'Praceta Antonio Leal de Oliveira','Carnaxide e Queijas').
paragem(1001,-104675.71,-95821.42,'Bom','Fechado dos Lados','Yes','Vimeca',[02,07,10,12],327,'Avenida do Forte','Carnaxide e Queijas').
paragem(607,-104700.62,-95803.69,'Bom','Fechado dos Lados','Yes','Vimeca',[02,07,10,12],327,'Avenida do Forte','Carnaxide e Queijas').
paragem(335,-106015.21,-96351.32,'Bom','Fechado dos Lados','Yes','Vimeca',[02,11],121,'Rua Joao Chagas','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(346,-105840.49,-96519.72,'Bom','Fechado dos Lados','Yes','Vimeca',[02,11],457,'Rua Francisco Jose Victorino','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(344,-105906.64,-96635.59,'Bom','Fechado dos Lados','Yes','Vimeca',[02,11],457,'Rua Francisco Jose Victorino','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(360,-105210.86,-96382.34,'Bom','Fechado dos Lados','Yes','Vimeca',[02,11],1283,'Avenida Vinte e Cinco de Abril de 1974','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(352,-105326.62,-96569.43,'Bom','Fechado dos Lados','Yes','Vimeca',[02,11],1283,'Avenida Vinte e Cinco de Abril de 1974','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(363,-106012.76,-96367.98,'Bom','Fechado dos Lados','Yes','Vimeca',[02,11,13],121,'Rua Joao Chagas','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(345,-105900.6,-96673.99,'Bom','Fechado dos Lados','Yes','Vimeca',[02,11,13],457,'Rua Francisco Jose Victorino','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(859,-105043.39,-96109.56,'Bom','Fechado dos Lados','Yes','Vimeca',[02,11,13],1283,'Avenida Vinte e Cinco de Abril de 1974','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(858,-105062.32,-96107.23,'Bom','Fechado dos Lados','Yes','Vimeca',[02,11,13],1283,'Avenida Vinte e Cinco de Abril de 1974','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(351,-105287.42,-96454.4,'Bom','Fechado dos Lados','Yes','Vimeca',[02,11,13],1283,'Avenida Vinte e Cinco de Abril de 1974','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(654,-106946.7,-95556.57,'Bom','Aberto dos Lados','No','Vimeca',[02,114],121,'Rua Joao Chagas','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(243,-104117.95,-97049.09,'Bom','Fechado dos Lados','Yes','Vimeca',[02,12,13,15],303,'Rua Cinco de Outubro','Carnaxide e Queijas').
paragem(248,-104091.69,-96778.69,'Bom','Fechado dos Lados','No','Vimeca',[02,12,13,15],362,'Largo da Patria Nova','Carnaxide e Queijas').
paragem(247,-104200.64,-96833.39,'Bom','Fechado dos Lados','Yes','Vimeca',[02,12,13,15],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(332,-105119.12,-97474.49,'Bom','Fechado dos Lados','Yes','Vimeca',[02,13,15],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(331,-105122.88,-97490.88,'Bom','Fechado dos Lados','Yes','Vimeca',[02,13,15],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(323,-105277.7,-97707.8,'Bom','Fechado dos Lados','Yes','Vimeca',[02,13,15],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(315,-105155.04,-98252.49,'Bom','Fechado dos Lados','Yes','Vimeca',[02,13,15],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(312,-105181.29,-98229.14,'Bom','Fechado dos Lados','No','SCoTTURB',[02,13,15],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(241,-104957.37,-97342.73,'Bom','Fechado dos Lados','Yes','Vimeca',[02,13,15],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(240,-104965.93,-97337.63,'Bom','Sem Abrigo','No','Vimeca',[02,13,15],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(239,-104604.14,-97197.81,'Bom','Fechado dos Lados','Yes','Vimeca',[02,13,15],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(238,-104609.35,-97210.07,'Bom','Sem Abrigo','No','SCoTTURB',[02,13,15],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(313,-105254.68,-97686.43,'Bom','Sem Abrigo','No','Vimeca',[02,13,15],1763,'Rua Visconde Moreira de Rey','Carnaxide e Queijas').
paragem(260,-104345.95,-97003.12,'Bom','Sem Abrigo','No','Vimeca',[02,15,13],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(246,-104328.14,-96988.84,'Bom','Sem Abrigo','No','Vimeca',[02,15,13],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(652,-106975.62,-95277.76,'Bom','Sem Abrigo','No','Vimeca',[06],103,'Rua Damiao de Gois','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(9,-107003,-95216.21,'Bom','Fechado dos Lados','Yes','Vimeca',[06],103,'Rua Damiao de Gois','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(6,-106992.24,-95299.38,'Bom','Sem Abrigo','No','Vimeca',[06],103,'Rua Damiao de Gois','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(886,-106634.688238017,-97653.97896394921,'Bom','Sem Abrigo','No','Vimeca',[06],382,'Avenida Pierre de Coubertin','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(473,-106717.49,-97337.39,'Bom','Sem Abrigo','No','Vimeca',[06],382,'Avenida Pierre de Coubertin','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(470,-106725.59,-97317.38,'Bom','Sem Abrigo','No','Vimeca',[06],382,'Avenida Pierre de Coubertin','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(904,-106835.46,-96672.9,'Bom','Fechado dos Lados','Yes','Vimeca',[06],386,'Rua Sacadura Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(902,-106880.68662292237,-96852.54363954351,'Bom','Sem Abrigo','No','Vimeca',[06],386,'Rua Sacadura Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(893,-106886.01,-96347.3,'Bom','Sem Abrigo','No','Vimeca',[06],386,'Rua Sacadura Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(483,-106784.91,-97126.09,'Bom','Sem Abrigo','No','Vimeca',[06],386,'Rua Sacadura Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(482,-106791.2,-97137.51,'Bom','Fechado dos Lados','Yes','SCoTTURB',[06],386,'Rua Sacadura Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(476,-106826.81,-96686.93,'Bom','Sem Abrigo','No','Vimeca',[06],386,'Rua Sacadura Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(472,-106866.01,-96904.64,'Bom','Sem Abrigo','No','Vimeca',[06],386,'Rua Sacadura Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(90,-103960,-96640.32,'Bom','Fechado dos Lados','Yes','Vimeca',[07,12,13,15],1113,'Avenida de Portugal','Carnaxide e Queijas').
paragem(30,-105300.44,-95336.46,'Bom','Fechado dos Lados','Yes','Vimeca',[10],113,'Alameda FerNo Lopes','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(29,-105256.47,-95349.66,'Bom','Fechado dos Lados','Yes','Vimeca',[10],113,'Alameda FerNo Lopes','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(20,-105353.27,-95172.19,'Bom','Fechado dos Lados','Yes','Vimeca',[10],113,'Alameda FerNo Lopes','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(646,-105261.03,-95520.31,'Bom','Sem Abrigo','No','Vimeca',[10],124,'Avenida Jose Gomes Ferreira','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(642,-105268.05,-95547.68,'Bom','Fechado dos Lados','Yes','Vimeca',[10],124,'Avenida Jose Gomes Ferreira','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(606,-104223.76,-95501.56,'Bom','Fechado dos Lados','Yes','Vimeca',[10],361,'Estrada de Outurela','Carnaxide e Queijas').
paragem(605,-104199.74,-95517.44,'Bom','Fechado dos Lados','Yes','Vimeca',[10],361,'Estrada de Outurela','Carnaxide e Queijas').
paragem(36,-105377.78526436003,-95633.40710368946,'Bom','Sem Abrigo','No','Vimeca',[10],416,'Alameda Antonio Sergio','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(17,-105324.76833309476,-95632.26166661376,'Bom','Sem Abrigo','No','Vimeca',[10],416,'Alameda Antonio Sergio','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(362,-105538.35,-96008.83,'Bom','Fechado dos Lados','Yes','Vimeca',[10,11,12],430,'Avenida Carolina Michaelis','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(28,-105593.51,-95907.44,'Bom','Aberto dos Lados','No','Vimeca',[10,11,12],430,'Avenida Carolina Michaelis','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(27,-105587.02,-95875.21,'Bom','Fechado dos Lados','Yes','Vimeca',[10,11,12,13],430,'Avenida Carolina Michaelis','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(26,-105556.0408335595,-95684.40583339432,'Bom','Sem Abrigo','No','Vimeca',[10,11,12,13],430,'Avenida Carolina Michaelis','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(688,-106112.34689956294,-95027.73434321095,'Bom','Fechado dos Lados','Yes','Vimeca',[10,12],10,'Avenida dos Bombeiros Voluntarios de Alges','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(679,-106071.42513405527,-95039.14634930693,'Bom','Fechado dos Lados','Yes','Vimeca',[10,12],10,'Avenida dos Bombeiros Voluntarios de Alges','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(675,-106288.85,-95136.57,'Bom','Fechado dos Lados','Yes','Vimeca',[10,12],10,'Avenida dos Bombeiros Voluntarios de Alges','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(671,-106568.5,-95165.9,'Bom','Fechado dos Lados','Yes','Vimeca',[10,12],10,'Avenida dos Bombeiros Voluntarios de Alges','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(75,-106560.62,-95186.03,'Bom','Aberto dos Lados','Yes','Vimeca',[10,12],10,'Avenida dos Bombeiros Voluntarios de Alges','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(72,-106342.5,-95131.58,'Bom','Fechado dos Lados','Yes','Vimeca',[10,12],10,'Avenida dos Bombeiros Voluntarios de Alges','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(526,-107061.05,-95215,'Bom','Fechado dos Lados','Yes','Vimeca',[10,12],102,'Largo Dom Manuel I','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(643,-105325.87,-95135.44,'Bom','Fechado dos Lados','Yes','Vimeca',[10,12],113,'Alameda FerNo Lopes','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(641,-105655.76,-95028.52,'Bom','Fechado dos Lados','Yes','Vimeca',[10,12],116,'Avenida General Norton de Matos','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(638,-105456.01,-94993.65,'Bom','Fechado dos Lados','Yes','Vimeca',[10,12],116,'Avenida General Norton de Matos','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(637,-105462.27,-94976.17,'Bom','Fechado dos Lados','Yes','Vimeca',[10,12],116,'Avenida General Norton de Matos','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(635,-105696.83,-95075.27,'Bom','Fechado dos Lados','Yes','Vimeca',[10,12],116,'Avenida General Norton de Matos','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(657,-106786.85846811837,-95149.7421827531,'Bom','Fechado dos Lados','Yes','Vimeca',[10,12],155,'Praca Doutor Manuel Martins','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(70,-106799.63,-95251.22,'Bom','Sem Abrigo','No','Vimeca',[10,12],155,'Praca Doutor Manuel Martins','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(349,-105225.66,-96048.66,'Bom','Fechado dos Lados','Yes','Vimeca',[10,12],407,'Rua Amaro Monteiro','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(860,-105051.07,-96033.67,'Bom','Fechado dos Lados','Yes','Vimeca',[10,12],416,'Alameda Antonio Sergio','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(361,-105510.18,-96017.31,'Bom','Fechado dos Lados','No','Vimeca',[10,12],430,'Avenida Carolina Michaelis','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(359,-105204.46,-96026.88,'Bom','Fechado dos Lados','Yes','Vimeca',[10,12],430,'Avenida Carolina Michaelis','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(37,-105549.20067076161,-95690.84269383312,'Bom','Fechado dos Lados','Yes','Vimeca',[10,12],430,'Avenida Carolina Michaelis','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(861,-105093.87,-96039.61,'Bom','Fechado dos Lados','Yes','Vimeca',[10,12,13],416,'Alameda Antonio Sergio','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(82,-104255.84,-95234.54,'Bom','Fechado dos Lados','No','Vimeca',[10,13,15],306,'Rua dos Cravos de Abril','Carnaxide e Queijas').
paragem(1013,-101793.68162303536,-99832.30867120415,'Bom','Sem Abrigo','No','LT',[101],217,'Rua da Constituicao','Barcarena').
paragem(102,-101969.18,-99801.53,'Bom','Sem Abrigo','No','LT',[101],217,'Rua da Constituicao','Barcarena').
paragem(101,-101994.64,-99805.01,'Bom','Sem Abrigo','No','LT',[101],217,'Rua da Constituicao','Barcarena').
paragem(106,-101762.99,-99819.05,'Bom','Sem Abrigo','No','LT',[101],261,'Rua da Juventude','Barcarena').
paragem(103,-101939.71,-99689.6,'Bom','Sem Abrigo','No','LT',[101],1003,'Rua Odette de Saint-Maurice','Barcarena').
paragem(737,-102409.39,-98701.67,'Bom','Fechado dos Lados','Yes','LT',[101,101,171],269,'Rua Mario Castelhano','Barcarena').
paragem(744,-102136.13485160771,-98663.30880207638,'Bom','Fechado dos Lados','No','LT',[101,102,106,171],216,'Estrada Consiglieri Pedroso','Barcarena').
paragem(715,-101966.52,-98573.78,'Bom','Fechado dos Lados','Yes','LT',[101,102,106,171],216,'Estrada Consiglieri Pedroso','Barcarena').
paragem(711,-101764.30649856283,-98424.15159847475,'Bom','Sem Abrigo','No','LT',[101,102,106,171],216,'Estrada Consiglieri Pedroso','Barcarena').
paragem(152,-102231.41,-98789.31,'Bom','Fechado dos Lados','Yes','LT',[101,102,106,171],216,'Estrada Consiglieri Pedroso','Barcarena').
paragem(127,-101949.9,-98542.91,'Bom','Fechado dos Lados','Yes','LT',[101,102,106,171],216,'Estrada Consiglieri Pedroso','Barcarena').
paragem(125,-101787.42,-98423.54,'Bom','Fechado dos Lados','Yes','LT',[101,102,106,171],216,'Estrada Consiglieri Pedroso','Barcarena').
paragem(732,-102381.73,-98965.83,'Bom','Sem Abrigo','No','LT',[101,102,106,171],950,'Estrada das Palmeiras','Barcarena').
paragem(733,-102638.72,-98781.31,'Bom','Sem Abrigo','No','LT',[101,102,171],993,'Rua do Trabalho','Barcarena').
paragem(146,-102407.34,-99102.68,'Bom','Sem Abrigo','No','LT',[101,106,171],216,'Estrada Consiglieri Pedroso','Barcarena').
paragem(145,-102412.85,-99137.94,'Bom','Fechado dos Lados','Yes','LT',[101,106,171],216,'Estrada Consiglieri Pedroso','Barcarena').
paragem(136,-102207.02,-99467.54,'Bom','Sem Abrigo','No','LT',[101,171],219,'Estrada da Cruz dos Cavalinhos','Barcarena').
paragem(135,-102185.42,-99474.62,'Bom','Sem Abrigo','No','LT',[101,171],219,'Estrada da Cruz dos Cavalinhos','Barcarena').
paragem(134,-102017.79,-99652.24,'Bom','Fechado dos Lados','Yes','LT',[101,171],219,'Estrada da Cruz dos Cavalinhos','Barcarena').
paragem(160,-102467.21,-98683.45,'Bom','Sem Abrigo','No','LT',[101,171],269,'Rua Mario Castelhano','Barcarena').
paragem(740,-102400.99,-98945.23,'Bom','Sem Abrigo','No','LT',[101,171],950,'Estrada das Palmeiras','Barcarena').
paragem(148,-102630.81,-98782.18,'Bom','Sem Abrigo','No','LT',[101,171],993,'Rua do Trabalho','Barcarena').
paragem(235,-104169.05,-97108.82,'Bom','Sem Abrigo','No','LT',[102],308,'Estrada do Desvio','Carnaxide e Queijas').
paragem(455,-106763.54,-97467.84,'Bom','Sem Abrigo','No','LT',[102],373,'Avenida Ferreira Godinho','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(454,-106758.19,-97475.72,'Bom','Sem Abrigo','No','LT',[102],373,'Avenida Ferreira Godinho','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(453,-106891.39,-97351.44,'Bom','Fechado dos Lados','No','LT',[102],373,'Avenida Ferreira Godinho','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(739,-102735.06,-98272.9,'Mau','Fechado dos Lados','No','Carris',[102],830,'Estrada Militar','Barcarena').
paragem(738,-103016.79,-98428.89,'Bom','Fechado dos Lados','Yes','LT',[102],830,'Estrada Militar','Barcarena').
paragem(690,-103002.83,-98398.75,'Bom','Aberto dos Lados','No','LT',[102],830,'Estrada Militar','Barcarena').
paragem(84,-102931.23,-98622.69,'Bom','Sem Abrigo','No','LT',[102],830,'Estrada Militar','Barcarena').
paragem(83,-102942.61,-98628.76,'Bom','Fechado dos Lados','Yes','Carris',[102],830,'Estrada Militar','Barcarena').
paragem(151,-102676.09,-98810.3,'Bom','Sem Abrigo','No','LT',[102],950,'Estrada das Palmeiras','Barcarena').
paragem(743,-102708.54,-98296.07,'Bom','Sem Abrigo','No','LT',[102],1099,'Rua Quinta da Bica do Sargento','Barcarena').
paragem(708,-103166.65231804183,-97987.56576748956,'Bom','Sem Abrigo','No','LT',[102],1200,'Rua Actor Carlos Cesar','Carnaxide e Queijas').
paragem(1016,-103193.05176985393,-97956.32085163088,'Bom','Sem Abrigo','No','LT',[102],1214,'Estrada de Queluz','Carnaxide e Queijas').
paragem(1015,-103181.82,-97967.06,'Bom','Sem Abrigo','No','LT',[102],1214,'Estrada de Queluz','Carnaxide e Queijas').
paragem(815,-104101.68,-97408.6,'Bom','Fechado dos Lados','Yes','LT',[102],1214,'Estrada de Queluz','Carnaxide e Queijas').
paragem(814,-104114.84,-97401.12,'Bom','Fechado dos Lados','Yes','LT',[102],1214,'Estrada de Queluz','Carnaxide e Queijas').
paragem(789,-103478.11,-97851.67,'Bom','Sem Abrigo','No','LT',[102],1214,'Estrada de Queluz','Carnaxide e Queijas').
paragem(169,-103468.05,-97872.21,'Bom','Fechado dos Lados','Yes','LT',[102],1214,'Estrada de Queluz','Carnaxide e Queijas').
paragem(158,-102845.12,-97961.08,'Bom','Sem Abrigo','No','LT',[102],1214,'Estrada de Queluz','Carnaxide e Queijas').
paragem(157,-102859.54,-97965.24,'Bom','Fechado dos Lados','Yes','LT',[102],1214,'Estrada de Queluz','Carnaxide e Queijas').
paragem(223,-104280.83,-98312.61,'Bom','Fechado dos Lados','No','LT',[102,103],1766,'Praceta Antonio Leal de Oliveira','Carnaxide e Queijas').
paragem(1009,-104303.63612383851,-98554.7783833525,'Bom','Sem Abrigo','No','LT',[102,108],79,'Rua dos Acores','Carnaxide e Queijas').
paragem(813,-104117.95,-97049.09,'Bom','Fechado dos Lados','Yes','LT',[102,108],303,'Rua Cinco de Outubro','Carnaxide e Queijas').
paragem(236,-104266.39,-96923.24,'Bom','Sem Abrigo','No','LT',[102,108],308,'Estrada do Desvio','Carnaxide e Queijas').
paragem(817,-104091.69,-96778.69,'Bom','Fechado dos Lados','No','LT',[102,108],362,'Largo da Patria Nova','Carnaxide e Queijas').
paragem(804,-104935.73,-98290.43,'Bom','Fechado dos Lados','Yes','LT',[102,108],813,'Rua Joao XXI','Carnaxide e Queijas').
paragem(803,-104768.69,-98266.88,'Bom','Fechado dos Lados','Yes','LT',[102,108],813,'Rua Joao XXI','Carnaxide e Queijas').
paragem(802,-104942.78,-98303.15,'Bom','Fechado dos Lados','Yes','LT',[102,108],813,'Rua Joao XXI','Carnaxide e Queijas').
paragem(632,-104801.2,-98279.24,'Bom','Fechado dos Lados','Yes','LT',[102,108],813,'Rua Joao XXI','Carnaxide e Queijas').
paragem(801,-104447.68,-98306.88,'Bom','Sem Abrigo','No','LT',[102,108],833,'Rua Mouzinho da Silveira','Carnaxide e Queijas').
paragem(842,-105122.88,-97490.88,'Bom','Fechado dos Lados','Yes','LT',[102,108],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(841,-105119.12,-97474.49,'Bom','Fechado dos Lados','Yes','LT',[102,108],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(838,-105277.7,-97707.8,'Bom','Fechado dos Lados','Yes','LT',[102,108],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(837,-105155.04,-98252.49,'Bom','Fechado dos Lados','Yes','LT',[102,108],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(835,-105181.29,-98229.14,'Bom','Fechado dos Lados','Yes','LT',[102,108],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(816,-104200.64,-96833.39,'Bom','Fechado dos Lados','Yes','LT',[102,108],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(811,-104957.37,-97342.73,'Bom','Fechado dos Lados','No','LT',[102,108],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(810,-104965.93,-97337.63,'Bom','Sem Abrigo','No','LT',[102,108],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(809,-104609.35,-97210.07,'Bom','Sem Abrigo','No','LT',[102,108],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(808,-104604.14,-97197.81,'Bom','Fechado dos Lados','Yes','LT',[102,108],1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas').
paragem(836,-105254.68,-97686.43,'Bom','Sem Abrigo','No','LT',[102,108],1763,'Rua Visconde Moreira de Rey','Carnaxide e Queijas').
paragem(805,-104471.99,-98565.73,'Bom','Fechado dos Lados','No','LT',[102,108,171],83,'Rua Angra do Heroismo','Carnaxide e Queijas').
paragem(800,-104618.82,-98507.86,'Bom','Fechado dos Lados','No','LT',[102,108,171],846,'Rua da Quinta do Bonfim','Carnaxide e Queijas').
paragem(229,-104718.77,-97838.97,'Bom','Fechado dos Lados','No','LT',[102,171],1767,'Rua Augusto Fraga','Carnaxide e Queijas').
paragem(581,-108611,-103212.55,'Bom','Fechado dos Lados','Yes','LT',[106],556,'Avenida Dom Joao I','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(576,-108633.94,-103087.73,'Bom','Fechado dos Lados','Yes','LT',[106],556,'Avenida Dom Joao I','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(941,-108629.88,-103387.36,'Bom','Fechado dos Lados','Yes','LT',[106],561,'Rua Dona Filipa de Lencastre','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(585,-108680.61,-103239.46,'Razoavel','Fechado dos Lados','Yes','LT',[106],561,'Rua Dona Filipa de Lencastre','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(780,-103217.78,-99786.02,'Bom','Fechado dos Lados','No','LT',[106],985,'Rua Sete de Junho','Barcarena').
paragem(967,-108145.87,-103052.15,'Bom','Fechado dos Lados','Yes','LT',[106],1332,'Rua da Fundicao de Oeiras','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(944,-108324.30768595074,-103189.2291270085,'Bom','Sem Abrigo','No','LT',[106],1344,'Avenida Infante Dom Henrique','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(579,-108585.23,-103414.87,'Bom','Sem Abrigo','No','LT',[106],1344,'Avenida Infante Dom Henrique','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(969,-108450.51,-102954.49,'Bom','Fechado dos Lados','Yes','LT',[106],1346,'Rua Infanta Dona Isabel','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(913,-108219.95,-102975.3,'Bom','Aberto dos Lados','No','LT',[106],1346,'Rua Infanta Dona Isabel','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(584,-108725.34,-103548.2,'Bom','Fechado dos Lados','No','LT',[106],1392,'Rua da Medrosa','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(583,-108734.22,-103555.55,'Bom','Fechado dos Lados','No','LT',[106],1392,'Rua da Medrosa','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(285,-105368.2,-101892.7,'Bom','Sem Abrigo','No','LT',[106],1497,'Avenida dos Bombeiros Voluntarios de Oeiras','Porto Salvo').
paragem(284,-105349.84,-101863.8,'Bom','Sem Abrigo','No','LT',[106],1497,'Avenida dos Bombeiros Voluntarios de Oeiras','Porto Salvo').
paragem(751,-103269.77,-101294.22,'Bom','Sem Abrigo','No','LT',[106,112,119],262,'Estrada de Leceia','Porto Salvo').
paragem(310,-107559.62,-102708.32,'Bom','Fechado dos Lados','Yes','LT',[106,111,112,115,122],514,'Largo Aviao Lusitania','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(505,-107655.06,-102500.24,'Bom','Fechado dos Lados','Yes','LT',[106,111,112,115,122],533,'Rua Candido dos Reis','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(501,-107752.93,-102745.45,'Bom','Fechado dos Lados','Yes','LT',[106,111,112,115,122],559,'Rua Desembargador Faria','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(540,-107146.31,-102052.84,'Bom','Fechado dos Lados','Yes','LT',[106,111,112,115,122],1325,'Rua da Figueirinha','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(521,-107653.99,-103018.24,'Bom','Fechado dos Lados','Yes','LT',[106,111,112,115,122],1372,'Rua dos Lagares da Quinta','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(208,-104277.99,-101693.69,'Bom','Fechado dos Lados','Yes','LT',[106,112],735,'Estrada de Leiao','Porto Salvo').
paragem(577,-108251.01,-102833.68,'Bom','Fechado dos Lados','Yes','LT',[106,112,115],494,'Largo Almirante Gago Coutinho','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(518,-107301.29,-102385.38,'Bom','Fechado dos Lados','Yes','LT',[106,112,115,122],578,'Avenida Embaixador Augusto de Castro','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(762,-103544.73,-101579.29,'Bom','Fechado dos Lados','Yes','LT',[106,112,119],735,'Estrada de Leiao','Porto Salvo').
paragem(756,-103586.35,-101579.63,'Bom','Fechado dos Lados','Yes','LT',[106,112,119],735,'Estrada de Leiao','Porto Salvo').
paragem(543,-107125.25,-102350.86,'Bom','Fechado dos Lados','Yes','LT',[106,112,122],545,'Praca Comandante Henrique Moreira Rato','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(10,-107129.12,-102327.55,'Bom','Fechado dos Lados','Yes','LT',[106,112,122],545,'Praca Comandante Henrique Moreira Rato','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(538,-107524.55,-102219.24,'Bom','Fechado dos Lados','Yes','LT',[106,112,122],1325,'Rua da Figueirinha','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(869,-106637.67595501819,-102220.03308837875,'Bom','Sem Abrigo','No','LT',[106,112,122],1407,'Estrada de Oeiras','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(542,-107049.08,-102098.31,'Bom','Fechado dos Lados','Yes','LT',[106,112,122],1427,'Avenida Rio de Janeiro','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(541,-107041.47,-102109.11,'Bom','Fechado dos Lados','Yes','LT',[106,112,122],1427,'Avenida Rio de Janeiro','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(516,-107095.35,-102502.91,'Bom','Fechado dos Lados','Yes','LT',[106,112,122],1427,'Avenida Rio de Janeiro','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(503,-107081.63,-102504.58,'Bom','Fechado dos Lados','Yes','LT',[106,112,122],1427,'Avenida Rio de Janeiro','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(828,-105046.86,-101627.86,'Bom','Fechado dos Lados','Yes','LT',[106,112,125,129],1540,'Estrada de Paco de Arcos','Porto Salvo').
paragem(797,-104431.06,-101723.48,'Bom','Fechado dos Lados','Yes','LT',[106,112,125,129,184],692,'Rua Conde de Rio Maior','Porto Salvo').
paragem(796,-104911.86,-101688.38,'Bom','Fechado dos Lados','Yes','LT',[106,112,125,129,184],692,'Rua Conde de Rio Maior','Porto Salvo').
paragem(795,-104741.4,-101691.52,'Bom','Fechado dos Lados','Yes','LT',[106,112,125,129,184],692,'Rua Conde de Rio Maior','Porto Salvo').
paragem(191,-104731,-101677.86,'Bom','Sem Abrigo','No','LT',[106,112,125,129,184],692,'Rua Conde de Rio Maior','Porto Salvo').
paragem(785,-103715.97,-100117.58,'Bom','Fechado dos Lados','Yes','LT',[106,117],242,'Largo General Humberto Delgado','Barcarena').
paragem(781,-103703.89,-100125.35,'Bom','Sem Abrigo','No','LT',[106,117],262,'Estrada de Leceia','Barcarena').
paragem(779,-103283.29,-99818.83,'Bom','Fechado dos Lados','No','Carris',[106,117],985,'Rua Sete de Junho','Barcarena').
paragem(774,-103410.59,-99904.77,'Bom','Fechado dos Lados','Yes','LT',[106,117],985,'Rua Sete de Junho','Barcarena').
paragem(773,-103414.27,-99913.2,'Bom','Sem Abrigo','No','LT',[106,117],985,'Rua Sete de Junho','Barcarena').
paragem(419,-106722.41,-99402.9,'Bom','Fechado dos Lados','No','LT',[106,117,158],849,'Avenida Antonio Florencio dos Santos','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(88,-106688.11,-99381.79,'Bom','Fechado dos Lados','Yes','LT',[106,117,158],849,'Avenida Antonio Florencio dos Santos','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(752,-103260.70410270982,-101287.68122082386,'Bom',_,'No','LT',[106,119],262,'Estrada de Leceia','Porto Salvo').
paragem(778,-103467.02,-100463.6,'Bom','Sem Abrigo','No','LT',[106,119],262,'Estrada de Leceia','Barcarena').
paragem(777,-103456.31,-100462.21,'Bom','Fechado dos Lados','No','LT',[106,119],262,'Estrada de Leceia','Barcarena').
paragem(776,-103364.8,-100773.19,'Bom','Sem Abrigo','No','LT',[106,119],262,'Estrada de Leceia','Barcarena').
paragem(775,-103358.57,-100763.83,'Bom','Fechado dos Lados','Yes','LT',[106,119],262,'Estrada de Leceia','Barcarena').
paragem(379,-106252.84,-102027.92,'Bom','Fechado dos Lados','Yes','LT',[106,122],1497,'Avenida dos Bombeiros Voluntarios de Oeiras','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(378,-106228.95,-102033.94,'Bom','Sem Abrigo','Yes','LT',[106,122],1497,'Avenida dos Bombeiros Voluntarios de Oeiras','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(294,-105880.9,-101989.75,'Bom','Fechado dos Lados','Yes','LT',[106,122],1497,'Avenida dos Bombeiros Voluntarios de Oeiras','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(283,-105571.85,-101959.97,'Bom','Fechado dos Lados','Yes','LT',[106,122],1497,'Avenida dos Bombeiros Voluntarios de Oeiras','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(282,-105530.56,-101934.24,'Bom','Fechado dos Lados','Yes','LT',[106,122],1497,'Avenida dos Bombeiros Voluntarios de Oeiras','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(281,-105866.86,-101977.3,'Bom','Fechado dos Lados','Yes','LT',[106,122],1497,'Avenida dos Bombeiros Voluntarios de Oeiras','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(693,-103934.24,-96642.56,'Bom','Fechado dos Lados','Yes','LT',[108],1113,'Avenida de Portugal','Carnaxide e Queijas').
paragem(692,-103960,-96640.32,'Bom','Fechado dos Lados','Yes','Carris',[108],1113,'Avenida de Portugal','Carnaxide e Queijas').
paragem(946,-103055.33836526402,-95462.37048401365,'Bom','Fechado dos Lados','No','Carris',[108,114],25,'Estrada de Alfragide',_).
paragem(592,-103100.09,-95100.64,'Razoavel','Fechado dos Lados','Yes','LT',[108,114],25,'Estrada de Alfragide','Carnaxide e Queijas').
paragem(591,-103097.89,-95148.46,'Razoavel','Fechado dos Lados','Yes','LT',[108,114],25,'Estrada de Alfragide','Carnaxide e Queijas').
paragem(590,-103055.84,-95605.42,'Bom','Sem Abrigo','No','LT',[108,114],25,'Estrada de Alfragide','Carnaxide e Queijas').
paragem(176,-103550.21,-96609.89,'Bom','Aberto dos Lados','Yes','LT',[108,114],276,'Estrada da Amadora','Carnaxide e Queijas').
paragem(175,-103543.27,-96685.43,'Bom','Fechado dos Lados','Yes','LT',[108,114],276,'Estrada da Amadora','Carnaxide e Queijas').
paragem(174,-103456.83,-96098.84,'Bom','Fechado dos Lados','No','LT',[108,114],276,'Estrada da Amadora','Carnaxide e Queijas').
paragem(173,-103441.79,-96114.45,'Bom','Fechado dos Lados','No','LT',[108,114],276,'Estrada da Amadora','Carnaxide e Queijas').
paragem(178,-103793.26,-96821.2,'Bom','Fechado dos Lados','Yes','LT',[108,114],1113,'Avenida de Portugal','Carnaxide e Queijas').
paragem(177,-103782.94,-96828.11,'Bom','Sem Abrigo','No','LT',[108,114],1113,'Avenida de Portugal','Carnaxide e Queijas').
paragem(327,-105824.71,-98610.29,'Bom','Sem Abrigo','No','LT',[108,115],830,'Estrada Militar','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(326,-105971.01,-98597.24,'Bom','Sem Abrigo','No','LT',[108,115],830,'Estrada Militar','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(325,-105660.54,-98769.86,'Bom','Fechado dos Lados','No','LT',[108,115],1796,'Rua das Ti-lias','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(324,-105658.22,-98790.53,'Bom','Fechado dos Lados','Yes','LT',[108,115],1796,'Rua das Ti-lias','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(431,-106095.33,-99310.37,'Bom','Fechado dos Lados','Yes','LT',[108,115,117],867,'Avenida Conselheiro Ferreira Lobo','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(274,-106013.52,-99221.37,'Bom','Fechado dos Lados','No','LT',[108,115,117,158],858,'Rua Calvet de Magalhaes','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(273,-106004.77,-99221.99,'Bom','Sem Abrigo','No','LT',[108,115,117,158],858,'Rua Calvet de Magalhaes','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(439,-106462.23,-99301.85,'Bom','Fechado dos Lados','No','LT',[108,115,117,158],867,'Avenida Conselheiro Ferreira Lobo','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(437,-106124.25,-99314.68,'Bom','Sem Abrigo','No','LT',[108,115,117,158],867,'Avenida Conselheiro Ferreira Lobo','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(409,-106998.97,-99255.62,'Bom','Fechado dos Lados','Yes','LT',[108,117],898,'Estrada da Gibalta','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(319,-105790.91,-99107.05,'Bom','Fechado dos Lados','Yes','LT',[108,117,115,158],909,'Avenida Joao de Freitas Branco','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(318,-105817.33,-99103.07,'Bom','Fechado dos Lados','Yes','LT',[108,117,115,158],909,'Avenida Joao de Freitas Branco','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(317,-105682.17,-99043.27,'Bom','Fechado dos Lados','Yes','LT',[108,117,115,158],909,'Avenida Joao de Freitas Branco','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(320,-105649.56,-98984.75,'Bom','Fechado dos Lados','Yes','LT',[108,117,115,158],936,'Largo da Quinta do Jardim','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(423,-106402.8,-99289.78,'Bom','Fechado dos Lados','Yes','LT',[108,117,158],1786,'Rua de Sao Joao de Deus','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(370,-105284.38,-95991.59,'Bom','Fechado dos Lados','Yes','SCoTTURB',[11],431,'Rua de Ceuta','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(348,-105643.02,-96045.85,'Bom','Aberto dos Lados','No','Vimeca',[11],442,'Rua Domingos Fernandes','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(18,-105326.04,-95824.84,'Bom','Fechado dos Lados','No','Vimeca',[11],443,'Rua Doutor Agostinho de Campos','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(340,-106068.28,-96585.41,'Bom','Sem Abrigo','No','Vimeca',[11],477,'Rua Luz Soriano','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(337,-105713.9,-96309.68,'Bom','Aberto dos Lados','No','Vimeca',[11],1251,'Rua Pedro Alvares Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(371,-105299.61,-95995.91,'Bom','Sem Abrigo','No','Vimeca',[11,13],431,'Rua de Ceuta','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(19,-105294.01,-95844.02,'Bom','Sem Abrigo','No','Vimeca',[11,13],431,'Rua de Ceuta','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(16,-105373.94,-95734.72,'Bom','Sem Abrigo','No','Vimeca',[11,13],431,'Rua de Ceuta','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(492,-106048.05,-96569.91,'Bom','Sem Abrigo','No','Vimeca',[11,13],477,'Rua Luz Soriano','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(338,-105719.88,-96325.39,'Bom','Aberto dos Lados','No','Vimeca',[11,13],1251,'Rua Pedro Alvares Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(875,-106837.97523209226,-101312.81293258877,'Bom','Fechado dos Lados','Yes','LT',[111],51,'Rua A Gazeta D Oeiras','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(874,-106828.30282704088,-101321.74130648235,'Bom','Fechado dos Lados','Yes','LT',[111],51,'Rua A Gazeta D Oeiras','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(549,-107045.27,-101540.24,'Bom','Fechado dos Lados','Yes','LT',[111],51,'Rua A Gazeta D Oeiras','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(548,-107036.05,-101530.9,'Bom','Fechado dos Lados','Yes','LT',[111],51,'Rua A Gazeta D Oeiras','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(404,-106707.27,-101357.94,'Bom','Sem Abrigo','No','LT',[111],51,'Rua A Gazeta D Oeiras','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(403,-106688.88,-101392.42,'Bom','Fechado dos Lados','Yes','LT',[111],51,'Rua A Gazeta D Oeiras','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(399,-106862.17,-101462.2,'Bom','Sem Abrigo','No','LT',[111],51,'Rua A Gazeta D Oeiras','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(398,-106850.3,-101488.95,'Bom','Sem Abrigo','No','LT',[111],51,'Rua A Gazeta D Oeiras','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(534,-107422.35,-102089.51,'Bom','Fechado dos Lados','No','Carris',[111],499,'Avenida de Angola','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(533,-107409.14,-102099.37,'Bom','Fechado dos Lados','No','LT',[111],499,'Avenida de Angola','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(532,-107471.58,-102018.18,'Bom','Aberto dos Lados','No','LT',[111],499,'Avenida de Angola','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(566,-107420.54,-102241.23,'Bom','Fechado dos Lados','Yes','LT',[111],547,'Rua Comandante Germano Dias','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(531,-107429.2,-102210.53,'Bom','Fechado dos Lados','Yes','LT',[111],547,'Rua Comandante Germano Dias','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(504,-107503.48,-102420.75,'Bom','Fechado dos Lados','Yes','LT',[111],547,'Rua Comandante Germano Dias','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(920,-107409.66,-102471.79,'Bom','Fechado dos Lados','Yes','LT',[111],551,'Avenida Copacabana','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(517,-107532.34,-102429.36,'Bom','Fechado dos Lados','Yes','LT',[111],551,'Avenida Copacabana','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(919,-107319.63,-102563.55,'Bom','Fechado dos Lados','Yes','LT',[111],578,'Avenida Embaixador Augusto de Castro','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(871,-106541.47,-101422.66,'Bom','Fechado dos Lados','Yes','LT',[111],587,'Avenida Antonio Bernardo Cabral de Macedo','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(408,-106741.29,-101198.09,'Bom','Fechado dos Lados','Yes','LT',[111],587,'Avenida Antonio Bernardo Cabral de Macedo','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(407,-106584.03,-101407.23,'Bom','Sem Abrigo','No','LT',[111],587,'Avenida Antonio Bernardo Cabral de Macedo','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(390,-106769.62,-101182.57,'Bom','Sem Abrigo','No','LT',[111],587,'Avenida Antonio Bernardo Cabral de Macedo','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(570,-107050.23,-100723.54,'Bom','Fechado dos Lados','Yes','LT',[111],605,'Rua Conde das Alcacovas','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(530,-107482.01,-102338.02,'Bom','Fechado dos Lados','Yes','LT',[111],1321,'Rua Fernando Pessoa','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(873,-106648.75190204488,-101501.00032816519,'Bom','Sem Abrigo','No','LT',[111],1359,'Rua Jose de Azambuja Proenca','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(394,-106663.26,-101486.4,'Bom','Sem Abrigo','No','LT',[111],1359,'Rua Jose de Azambuja Proenca','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(567,-107279.28,-102025.92,'Bom','Fechado dos Lados','Yes','LT',[111],1455,'Avenida do Ultramar','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(539,-107434.4,-101991.91,'Bom','Fechado dos Lados','Yes','LT',[111],1455,'Avenida do Ultramar','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(1025,-108102.81093469674,-103074.8246594517,'Bom','Sem Abrigo','No','LT',[111, 122],1342,'Rua Henrique de Paiva Couceiro','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(554,-107124.01,-101962.87,'Bom','Fechado dos Lados','Yes','LT',[111,115],527,'Avenida de Brasilia','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(553,-107113.59,-101968.28,'Bom','Fechado dos Lados','Yes','LT',[111,115],527,'Avenida de Brasilia','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(552,-107114.28,-101783.86,'Bom','Fechado dos Lados','Yes','LT',[111,115],527,'Avenida de Brasilia','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(551,-107102.8,-101781.42,'Bom','Fechado dos Lados','Yes','LT',[111,115],527,'Avenida de Brasilia','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(565,-107105.26,-101627.34,'Bom','Sem Abrigo','No','LT',[111,115],601,'Rua Carlos Vieira Ramos','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(564,-107094.68,-101630.41,'Bom','Sem Abrigo','No','LT',[111,115],601,'Rua Carlos Vieira Ramos','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(925,-107625.08,-103117.77,'Bom','Fechado dos Lados','No','LT',[111,122],1431,'Avenida Salvador Allende','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(872,-106368.26,-101705.73,'Bom','Sem Abrigo','Yes','LT',[111,158],587,'Avenida Antonio Bernardo Cabral de Macedo','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(391,-106420.98,-101611.2,'Bom','Fechado dos Lados','Yes','LT',[111,158],587,'Avenida Antonio Bernardo Cabral de Macedo','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(705,-101884.93,-101826.65,'Bom','Fechado dos Lados','Yes','LT',[112],1667,'Avenida Professor Dr. Cavaco Silva','Porto Salvo').
paragem(704,-101856.51,-101822.02,'Bom','Fechado dos Lados','Yes','LT',[112],1667,'Avenida Professor Dr. Cavaco Silva','Porto Salvo').
paragem(272,-105722.56,-102581.2,'Bom','Sem Abrigo','No','LT',[112],1680,'Rua Encosta das Lagoas','Porto Salvo').
paragem(271,-105742.74,-102575.8,'Bom','Fechado dos Lados','No','LT',[112],1680,'Rua Encosta das Lagoas','Porto Salvo').
paragem(727,-102515.87,-101878.09,'Bom','Fechado dos Lados','Yes','LT',[112,119,125],1667,'Avenida Professor Dr. Cavaco Silva','Porto Salvo').
paragem(726,-102509.72,-101859.8,'Bom','Fechado dos Lados','Yes','LT',[112,119,125],1667,'Avenida Professor Dr. Cavaco Silva','Porto Salvo').
paragem(723,-102865.58,-101399.39,'Bom','Fechado dos Lados','Yes','LT',[112,119,125],1667,'Avenida Professor Dr. Cavaco Silva','Porto Salvo').
paragem(722,-102849.51,-101421.76,'Bom','Fechado dos Lados','Yes','LT',[112,119,125],1667,'Avenida Professor Dr. Cavaco Silva','Porto Salvo').
paragem(717,-102227.22,-101894.71,'Bom','Fechado dos Lados','Yes','LT',[112,119,125],1667,'Avenida Professor Dr. Cavaco Silva','Porto Salvo').
paragem(716,-102227.55,-101920.36,'Bom','Fechado dos Lados','Yes','LT',[112,119,125],1667,'Avenida Professor Dr. Cavaco Silva','Porto Salvo').
paragem(832,-105236.25,-102190.54,'Bom','Fechado dos Lados','Yes','LT',[112,122],1682,'Avenida Santa Casa da Misericordia de Oeiras','Porto Salvo').
paragem(831,-105274.84,-101913.18,'Bom','Fechado dos Lados','Yes','LT',[112,122],1682,'Avenida Santa Casa da Misericordia de Oeiras','Porto Salvo').
paragem(830,-105227.47,-102176.58,'Bom','Fechado dos Lados','Yes','LT',[112,122],1682,'Avenida Santa Casa da Misericordia de Oeiras','Porto Salvo').
paragem(829,-105291.98,-101912.29,'Bom','Fechado dos Lados','Yes','LT',[112,122],1682,'Avenida Santa Casa da Misericordia de Oeiras','Porto Salvo').
paragem(989,-106533.85390436777,-102159.09374561995,'Bom','Aberto dos Lados','No','LT',[112,122,106],1407,'Estrada de Oeiras','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(827,-105268.41,-102428.49,'Bom','Fechado dos Lados','Yes','LT',[112,122,129],1680,'Rua Encosta das Lagoas','Porto Salvo').
paragem(266,-105280.44,-102478.21,'Bom','Fechado dos Lados','Yes','LT',[112,122,129],1680,'Rua Encosta das Lagoas','Porto Salvo').
paragem(381,-106248.58,-102114.98,'Razoavel','Sem Abrigo','No','LT',[112,158],1521,'Estrada da Ribeira da Laje','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(380,-106217.2,-102161.99,'Bom','Fechado dos Lados','Yes','LT',[112,158],1521,'Estrada da Ribeira da Laje','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(81,-107028.61,-95211.28,'Bom','Fechado dos Lados','Yes','LT',[114],103,'Rua Damiao de Gois','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(64,-106263.4,-95432.65,'Bom','Fechado dos Lados','Yes','LT',[114],142,'Avenida da Republica','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(63,-106281.59,-95428.61,'Bom','Sem Abrigo','No','LT',[114],142,'Avenida da Republica','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(62,-106448.94,-95449.29,'Bom','Fechado dos Lados','Yes','LT',[114],142,'Avenida da Republica','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(59,-106702.76,-95584.31,'Bom','Sem Abrigo','No','LT',[114],142,'Avenida da Republica','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(58,-106491,-95464.18,'Bom','Fechado dos Lados','Yes','LT',[114],142,'Avenida da Republica','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(57,-106676.23,-95569.51,'Bom','Fechado dos Lados','No','LT',[114],142,'Avenida da Republica','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(364,-105749.92,-96128.02,'Bom','Fechado dos Lados','Yes','LT',[114],150,'Rua Victor Duarte Pedroso','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(61,-106056.3,-95443.94,'Bom','Fechado dos Lados','No','LT',[114],150,'Rua Victor Duarte Pedroso','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(60,-106031.48,-95429.88,'Bom','Sem Abrigo','No','LT',[114],150,'Rua Victor Duarte Pedroso','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(33,-105923.57,-95719.27,'Bom','Fechado dos Lados','No','LT',[114],150,'Rua Victor Duarte Pedroso','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(32,-105942.11,-95669.45,'Bom','Fechado dos Lados','No','LT',[114],150,'Rua Victor Duarte Pedroso','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(1002,-104226.49,-95797.22,'Bom','Fechado dos Lados','Yes','LT',[114],327,'Avenida do Forte','Carnaxide e Queijas').
paragem(986,-104675.71,-95821.42,'Bom','Fechado dos Lados','Yes','LT',[114],327,'Avenida do Forte','Carnaxide e Queijas').
paragem(983,-104700.62,-95803.69,'Bom','Fechado dos Lados','Yes','LT',[114],327,'Avenida do Forte','Carnaxide e Queijas').
paragem(977,-104296.72,-95828.26,'Bom','Fechado dos Lados','Yes','LT',[114],327,'Avenida do Forte','Carnaxide e Queijas').
paragem(950,-103725.69,-95975.2,'Bom','Fechado dos Lados','Yes','LT',[114],354,'Rua Manuel Teixeira Gomes','Carnaxide e Queijas').
paragem(792,-103922.82,-96235.62,'Bom','Fechado dos Lados','Yes','LT',[114],354,'Rua Manuel Teixeira Gomes','Carnaxide e Queijas').
paragem(333,-105712.14,-96154.74,'Bom','Fechado dos Lados','Yes','LT',[114],450,'Rua Engenheiro Jose Frederico Ulrich','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(823,-104032.88,-96536.98,'Bom','Fechado dos Lados','Yes','LT',[114],1113,'Avenida de Portugal','Carnaxide e Queijas').
paragem(818,-104031.08,-96173.83,'Bom','Fechado dos Lados','Yes','LT',[114],1113,'Avenida de Portugal','Carnaxide e Queijas').
paragem(807,-104003.78,-96559.17,'Bom','Fechado dos Lados','Yes','LT',[114],1113,'Avenida de Portugal','Carnaxide e Queijas').
paragem(710,-103972.32,-95981.88,'Bom','Fechado dos Lados','Yes','LT',[114],1113,'Avenida de Portugal','Carnaxide e Queijas').
paragem(954,-104075.89,-95771.82,'Bom','Fechado dos Lados','Yes','LT',[114],1116,'Avenida Professor Dr. Reinaldo dos Santos','Carnaxide e Queijas').
paragem(947,-103879.91,-95751.23,'Bom','Fechado dos Lados','No','LT',[114],1116,'Avenida Professor Dr. Reinaldo dos Santos','Carnaxide e Queijas').
paragem(952,-104058.98,-95839.14,'Bom','Fechado dos Lados','Yes','LT',[114],1137,'Rua Tenente-General Zeferino Sequeira','Carnaxide e Queijas').
paragem(846,-105713.9,-96309.68,'Bom','Aberto dos Lados','No','LT',[114],1251,'Rua Pedro Alvares Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(845,-105719.88,-96325.39,'Bom','Aberto dos Lados','No','LT',[114],1251,'Rua Pedro Alvares Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(367,-105679.92,-96534.6,'Bom','Fechado dos Lados','Yes','LT',[114],1279,'Avenida Tomas Ribeiro','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(330,-105744.42,-96527.5,'Bom','Fechado dos Lados','Yes','LT',[114],1279,'Avenida Tomas Ribeiro','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(863,-105210.86,-96382.34,'Bom','Fechado dos Lados','Yes','LT',[114],1283,'Avenida Vinte e Cinco de Abril de 1974','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(857,-105326.62,-96569.43,'Bom','Fechado dos Lados','Yes','LT',[114],1283,'Avenida Vinte e Cinco de Abril de 1974','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(856,-105287.42,-96454.4,'Bom','Fechado dos Lados','Yes','LT',[114],1283,'Avenida Vinte e Cinco de Abril de 1974','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(354,-105043.39,-96109.56,'Bom','Fechado dos Lados','Yes','LT',[114],1283,'Avenida Vinte e Cinco de Abril de 1974','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(353,-105062.32,-96107.23,'Bom','Fechado dos Lados','Yes','LT',[114],1283,'Avenida Vinte e Cinco de Abril de 1974','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(535,-107581.42,-102198.59,'Bom','Fechado dos Lados','Yes','LT',[115],533,'Rua Candido dos Reis','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(1006,-107164.33866025771,-101426.22549078583,'Bom','Fechado dos Lados','Yes','LT',[115],601,'Rua Carlos Vieira Ramos','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(563,-107141.23,-101485.07,'Bom','Fechado dos Lados','Yes','LT',[115],601,'Rua Carlos Vieira Ramos','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(425,-106665.8,-99468.51,'Bom','Fechado dos Lados','No','LT',[115],856,'Rua Bernardim Ribeiro','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(432,-106389.99,-99441.49,'Bom','Aberto dos Lados','No','LT',[115],899,'Rua de Goa','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(424,-106482.29,-99626.9,'Bom','Aberto dos Lados','No','LT',[115],899,'Rua de Goa','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(934,-107286.31,-102095.09,'Bom','Fechado dos Lados','Yes','LT',[115],1325,'Rua da Figueirinha','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(568,-107459.5,-101976.24,'Bom','Fechado dos Lados','Yes','LT',[115],1455,'Avenida do Ultramar','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(562,-107112,-101075.39,'Bom','Fechado dos Lados','No','LT',[115],1527,'Rua Manuel Pinhancos','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(429,-106431.59,-99785.79,'Bom','Sem Abrigo','No','LT',[115],1769,'Rua Vasco da Gama','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(428,-106433.33,-99804.88,'Bom','Sem Abrigo','No','LT',[115],1769,'Rua Vasco da Gama','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(1008,-106840.5909766439,-100709.59335706284,'Bom','Sem Abrigo','No','LT',[115,158],585,'Rua de Angola','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(436,-106547.04,-100175.4,'Bom','Fechado dos Lados','Yes','LT',[115,158],592,'Rua Augusto Sousa Lobo','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(435,-106550.9,-100185.96,'Bom','Aberto dos Lados','No','LT',[115,158],592,'Rua Augusto Sousa Lobo','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(421,-106355.98,-100328.5,'Bom','Fechado dos Lados','No','LT',[115,158],592,'Rua Augusto Sousa Lobo','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(420,-106369.82,-100337.94,'Bom','Fechado dos Lados','No','LT',[115,158],592,'Rua Augusto Sousa Lobo','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(413,-106399.4,-100625.78,'Bom','Fechado dos Lados','No','LT',[115,158],609,'Rua Conde de Rio Maior','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(442,-106329.69,-100480.44,'Bom','Fechado dos Lados','Yes','LT',[115,158],610,'Avenida Conde de Sao Januario','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(412,-106824.4,-100718.89,'Bom','Fechado dos Lados','No','LT',[115,158],610,'Avenida Conde de Sao Januario','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(411,-106318.44,-100465.14,'Bom','Aberto dos Lados','No','LT',[115,158],610,'Avenida Conde de Sao Januario','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(402,-106864.28,-101108.52,'Bom','Fechado dos Lados','Yes','LT',[115,158],620,'Avenida Elvira Velez','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(401,-106863.16,-101122.04,'Bom','Fechado dos Lados','No','LT',[115,158],620,'Avenida Elvira Velez','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(418,-106441.72,-100682.43,'Bom','Aberto dos Lados','No','LT',[115,158],645,'Rua Instituto Conde de Agrolongo','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(415,-106605.36,-100689.48,'Bom','Sem Abrigo','No','LT',[115,158],645,'Rua Instituto Conde de Agrolongo','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(414,-106613.41,-100706.91,'Bom','Fechado dos Lados','No','LT',[115,158],645,'Rua Instituto Conde de Agrolongo','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(427,-106642.38,-99930.18,'Bom','Aberto dos Lados','No','LT',[115,158],1785,'Rua de Sao Gabriel','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(426,-106638.34,-99940.41,'Bom','Aberto dos Lados','No','LT',[115,158],1785,'Rua de Sao Gabriel','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(1011,-107442.97396256146,-100964.28382638063,'Bom','Fechado dos Lados','Yes','LT',[116],611,'Rua Costa Pinto','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(555,-107479.04447916782,-101162.71630208207,'Bom','Sem Abrigo','No','LT',[116],611,'Rua Costa Pinto','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(525,-107487.42,-101137.8,'Bom','Fechado dos Lados','Yes','LT',[116],611,'Rua Costa Pinto','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(569,-107425.69,-101005.38,'Bom','Fechado dos Lados','Yes','LT',[116],614,'Praceta Dionisio Matias','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(935,-107842.63,-101657.47,'Bom','Sem Abrigo','No','LT',[116],622,'Avenida Engenheiro Bonneville Franco','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(547,-107742.46,-101446.79,'Bom','Fechado dos Lados','Yes','LT',[116],622,'Avenida Engenheiro Bonneville Franco','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(546,-107715.55,-101440.41,'Bom','Sem Abrigo','No','LT',[116],622,'Avenida Engenheiro Bonneville Franco','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(545,-107855.7,-101644.74,'Bom','Fechado dos Lados','Yes','LT',[116],622,'Avenida Engenheiro Bonneville Franco','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(987,-107154.05859701223,-100851.89900138501,'Bom','Fechado dos Lados','Yes','LT',[116],1569,'Avenida Senhor Jesus dos Navegantes','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(524,-107115.55,-100887.65,'Bom','Fechado dos Lados','Yes','LT',[116],1569,'Avenida Senhor Jesus dos Navegantes','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(787,-103931.57,-99415.05,'Bom','Sem Abrigo','No','LT',[117],201,'Estrada do Cacem','Barcarena').
paragem(218,-104375.85,-99328,'Bom','Sem Abrigo','No','LT',[117],201,'Estrada do Cacem','Barcarena').
paragem(217,-104361.42,-99334.49,'Bom','Sem Abrigo','No','LT',[117],201,'Estrada do Cacem','Barcarena').
paragem(216,-104587.38,-99431.91,'Bom','Sem Abrigo','No','LT',[117],201,'Estrada do Cacem','Barcarena').
paragem(215,-104638.85,-99443.96,'Bom','Sem Abrigo','No','LT',[117],201,'Estrada do Cacem','Barcarena').
paragem(124,-101302.34,-99804.3,'Bom','Fechado dos Lados','No','LT',[117],252,'Avenida Infante Dom Henrique','Barcarena').
paragem(123,-101315.5,-99829.06,'Bom','Fechado dos Lados','No','LT',[117],252,'Avenida Infante Dom Henrique','Barcarena').
paragem(117,-101493.79,-99968.88,'Bom','Fechado dos Lados','Yes','LT',[117],252,'Avenida Infante Dom Henrique','Barcarena').
paragem(771,-103918.36,-99410.5,'Bom','Fechado dos Lados','No','LT',[117],256,'Rua Joaquim Sabino de Sousa','Barcarena').
paragem(769,-103650.67,-99459.31,'Bom','Fechado dos Lados','No','LT',[117],256,'Rua Joaquim Sabino de Sousa','Barcarena').
paragem(768,-103643.5,-99453.56,'Bom','Sem Abrigo','No','LT',[117],256,'Rua Joaquim Sabino de Sousa','Barcarena').
paragem(448,-106045.74,-98590.38,'Bom','Sem Abrigo','No','LT',[117],830,'Estrada Militar','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(447,-106415.98,-98591.36,'Bom','Sem Abrigo','No','LT',[117],830,'Estrada Militar','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(446,-106025.83,-98604.24,'Bom','Sem Abrigo','No','LT',[117],830,'Estrada Militar','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(452,-105233.04,-99103.06,'Bom','Sem Abrigo','No','LT',[117],925,'Estrada do Murganhal','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(445,-106027.77,-98850.69,'Bom','Fechado dos Lados','Yes','LT',[117],925,'Estrada do Murganhal','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(444,-106017.28,-98852.36,'Bom','Sem Abrigo','No','LT',[117],925,'Estrada do Murganhal','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(322,-105424.33,-99044.64,'Bom','Fechado dos Lados','Yes','LT',[117],925,'Estrada do Murganhal','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(321,-105462.35,-99002.28,'Bom','Fechado dos Lados','No','LT',[117],925,'Estrada do Murganhal','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(214,-104907.67,-99367.84,'Bom','Sem Abrigo','No','LT',[117],925,'Estrada do Murganhal','Barcarena').
paragem(450,-106594.42,-99048.6,'Bom','Sem Abrigo','No','LT',[117,158],882,'Rua Doutor Jorge Rivotti','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(449,-106581.18,-99035.41,'Bom','Fechado dos Lados','No','LT',[117,158],882,'Rua Doutor Jorge Rivotti','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(299,-105855.31,-99342.8,'Bom','Fechado dos Lados','No','LT',[117,158],1805,'Rua Dona Yesoa Godinho','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(770,-103163.15,-99786.4,'Bom','Fechado dos Lados','Yes','LT',[117,171],201,'Estrada do Cacem','Barcarena').
paragem(138,-102814.25,-99907.47,'Mau','Fechado dos Lados','No','LT',[117,171],201,'Estrada do Cacem','Barcarena').
paragem(137,-102792.58,-99921.93,'Bom','Sem Abrigo','No','LT',[117,171],201,'Estrada do Cacem','Barcarena').
paragem(766,-103346.29,-99565.78,'Bom','Fechado dos Lados','Yes','LT',[117,171],210,'Largo Cinco de Outubro','Barcarena').
paragem(767,-103244.97,-99729.51,'Bom','Fechado dos Lados','Yes','LT',[117,171],235,'Rua Felner Duarte','Barcarena').
paragem(143,-102122.63,-99975.95,'Bom','Sem Abrigo','No','LT',[117,171],241,'Estrada das Fontainhas','Barcarena').
paragem(142,-102137.2,-99979.69,'Bom','Sem Abrigo','No','LT',[117,171],241,'Estrada das Fontainhas','Barcarena').
paragem(140,-102285.58,-100095.76,'Bom','Sem Abrigo','No','LT',[117,171],241,'Estrada das Fontainhas','Barcarena').
paragem(139,-102277.41,-100088.41,'Bom','Sem Abrigo','No','LT',[117,171],241,'Estrada das Fontainhas','Barcarena').
paragem(116,-101520.29,-100001.26,'Bom','Fechado dos Lados','Yes','Carris',[117,171],252,'Avenida Infante Dom Henrique','Barcarena').
paragem(691,-103349.27,-99588.57,'Bom','Sem Abrigo','No','LT',[117,171],964,'Jardim Publico','Barcarena').
paragem(141,-102028.47,-99961.71,'Bom','Sem Abrigo','No','LT',[117,171],978,'Avenida de Santo Antonio de Tercena','Barcarena').
paragem(122,-102021.07,-99964.5,'Bom','Sem Abrigo','No','LT',[117,171],978,'Avenida de Santo Antonio de Tercena','Barcarena').
paragem(121,-101894.85,-100053.16,'Bom','Sem Abrigo','No','LT',[117,171],978,'Avenida de Santo Antonio de Tercena','Barcarena').
paragem(120,-101884.83,-100069.82,'Bom','Fechado dos Lados','Yes','LT',[117,171],978,'Avenida de Santo Antonio de Tercena','Barcarena').
paragem(119,-101709.63,-100014.88,'Bom','Fechado dos Lados','Yes','LT',[117,171],978,'Avenida de Santo Antonio de Tercena','Barcarena').
paragem(118,-101728.86,-100021.08,'Bom','Fechado dos Lados','No','LT',[117,171],978,'Avenida de Santo Antonio de Tercena','Barcarena').
paragem(786,-103718.6,-100106.34,'Bom','Sem Abrigo','No','LT',[119],242,'Largo General Humberto Delgado','Barcarena').
paragem(784,-103738.83,-100125.9,'Bom','Sem Abrigo','No','LT',[119],242,'Largo General Humberto Delgado','Barcarena').
paragem(772,-103786.35,-100195.03,'Bom','Sem Abrigo','No','LT',[119],244,'Rua Gil Vicente','Barcarena').
paragem(783,-103895.41,-100162.43,'Bom','Sem Abrigo','No','LT',[119],274,'Rua do Moinho','Barcarena').
paragem(1018,-107041.77476893747,-101229.91096074758,'Bom','Sem Abrigo','No','LT',[119],620,'Avenida Elvira Velez','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(694,-104519.7809297323,-100793.13013878663,'Bom','Sem Abrigo','No','LT',[119],679,'Rua Artur Moura','Porto Salvo').
paragem(731,-103012.13,-102009.23,'Bom','Sem Abrigo','No','LT',[119],711,'Rua Fernando Sabido','Porto Salvo').
paragem(782,-103699.86,-100194.78,'Bom','Fechado dos Lados','Yes','LT',[119],1001,'Rua da Fonte','Barcarena').
paragem(222,-104759.2,-100697.28,'Bom','Sem Abrigo','No','LT',[119],1640,'Avenida Vinte e Cinco de Abril','Porto Salvo').
paragem(213,-104589.54,-100696.1,'Bom','Fechado dos Lados','No','LT',[119],1640,'Avenida Vinte e Cinco de Abril','Porto Salvo').
paragem(707,-102002.37,-102008.48,'Bom','Fechado dos Lados','No','LT',[119],1668,'Avenida Engenheiro Valente de Oliveira','Porto Salvo').
paragem(132,-102642.99,-102233.26,'Bom','Sem Abrigo','No','LT',[119],1668,'Avenida Engenheiro Valente de Oliveira','Porto Salvo').
paragem(99,-101995.52,-102016.59,'Bom','Fechado dos Lados','No','LT',[119],1668,'Avenida Engenheiro Valente de Oliveira','Porto Salvo').
paragem(130,-102992.86,-102011.13,'Bom','Sem Abrigo','No','LT',[119],1670,'Avenida Domingos Vandelli','Porto Salvo').
paragem(725,-102556.6,-102172.39,'Bom','Sem Abrigo','No','LT',[119],1671,'Rua Professor Dr. Jose Pinto Peixoto','Porto Salvo').
paragem(441,-107020.04,-100736.2,'Bom','Fechado dos Lados','Yes','LT',[119,115],605,'Rua Conde das Alcacovas','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(750,-103210.92,-101837,'Bom','Fechado dos Lados','No','LT',[119,125],1634,'Estrada de Talai-de','Porto Salvo').
paragem(749,-103174.51,-101870.25,'Bom','Sem Abrigo','No','LT',[119,125],1634,'Estrada de Talai-de','Porto Salvo').
paragem(748,-103481.37,-101650.92,'Bom','Fechado dos Lados','No','LT',[119,125],1634,'Estrada de Talai-de','Porto Salvo').
paragem(757,-103529.69,-101634.82,'Bom','Sem Abrigo','No','LT',[119,125],1661,'Rua Henrique Marques','Porto Salvo').
paragem(98,-101970.18,-101783.3,'Bom','Sem Abrigo','No','LT',[119,125],1667,'Avenida Professor Dr. Cavaco Silva','Porto Salvo').
paragem(97,-101959.47,-101795.46,'Bom','Sem Abrigo','No','LT',[119,125],1667,'Avenida Professor Dr. Cavaco Silva','Porto Salvo').
paragem(389,-106757.03,-100945.88,'Bom','Fechado dos Lados','Yes','LT',[119,125,129,158,184],1540,'Estrada de Paco de Arcos','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(388,-106176.2,-101085.06,'Bom','Fechado dos Lados','Yes','LT',[119,125,129,158,184],1540,'Estrada de Paco de Arcos','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(387,-106378.14,-101089.06,'Bom','Fechado dos Lados','Yes','LT',[119,125,129,158,184],1540,'Estrada de Paco de Arcos','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(386,-106385.21,-101073.57,'Bom','Sem Abrigo','No','LT',[119,125,129,158,184],1540,'Estrada de Paco de Arcos','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(385,-106674.29,-100994.88,'Bom','Fechado dos Lados','Yes','LT',[119,125,129,158,184],1540,'Estrada de Paco de Arcos','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(1004,-105463.93407291333,-101208.08123805858,'Bom','Fechado dos Lados','Yes','LT',[119,125,129,184],1540,'Estrada de Paco de Arcos','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(290,-105761.18,-101097.27,'Bom','Fechado dos Lados','Yes','LT',[119,125,129,184],1540,'Estrada de Paco de Arcos','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(288,-106033.78,-101107.92,'Bom','Fechado dos Lados','Yes','LT',[119,125,129,184],1540,'Estrada de Paco de Arcos','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(287,-105447.82,-101232.23,'Bom','Fechado dos Lados','Yes','LT',[119,125,129,184],1540,'Estrada de Paco de Arcos','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(289,-105894.4,-101171.62,'Bom','Fechado dos Lados','Yes','LT',[119,125,129,184],1596,'Rua Shegundo Galarza','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(277,-105302.37,-101111.62,'Bom','Fechado dos Lados','No','LT',[119,184],70,'Rua Actor Antonio Pinheiro','Porto Salvo').
paragem(276,-105287.53,-101100.79,'Bom','Sem Abrigo','No','LT',[119,184],70,'Rua Actor Antonio Pinheiro','Porto Salvo').
paragem(259,-104635.54,-100919.19,'Bom','Fechado dos Lados','Yes','LT',[119,184],685,'Rua Carlos Paiao','Porto Salvo').
paragem(258,-105092.32,-100957.4,'Bom','Sem Abrigo','No','Carris',[119,184],1640,'Avenida Vinte e Cinco de Abril','Porto Salvo').
paragem(257,-105076.45,-100946.89,'Bom','Sem Abrigo','No','LT',[119,184],1640,'Avenida Vinte e Cinco de Abril','Porto Salvo').
paragem(109,-104899.42,-100859.7,'Bom','Fechado dos Lados','No','LT',[119,184],1640,'Avenida Vinte e Cinco de Abril','Porto Salvo').
paragem(108,-104913.48,-100837.78,'Bom','Fechado dos Lados','No','LT',[119,184],1640,'Avenida Vinte e Cinco de Abril','Porto Salvo').
paragem(34,-105634.78,-95513.74,'Bom','Fechado dos Lados','Yes','Vimeca',[12],120,'Avenida Jaime CorteSao','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(22,-105577.35,-95503.97,'Bom','Fechado dos Lados','Yes','Vimeca',[12],120,'Avenida Jaime CorteSao','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(25,-105539.98,-95177.67,'Bom','Sem Abrigo','No','Vimeca',[12],148,'Avenida das Tulipas','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(24,-105556.38,-95196.87,'Bom','Fechado dos Lados','Yes','Vimeca',[12],148,'Avenida das Tulipas','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(714,-101949.9,-98542.91,'Bom','Fechado dos Lados','Yes','Vimeca',[12],216,'Estrada Consiglieri Pedroso','Barcarena').
paragem(713,-101787.42,-98423.54,'Bom','Fechado dos Lados','Yes','Vimeca',[12],216,'Estrada Consiglieri Pedroso','Barcarena').
paragem(712,-101764.30649856283,-98424.15159847475,'Bom','Sem Abrigo','No','SCoTTURB',[12],216,'Estrada Consiglieri Pedroso','Barcarena').
paragem(153,-102409.39,-98701.67,'Bom','Fechado dos Lados','Yes','Vimeca',[12],269,'Rua Mario Castelhano','Barcarena').
paragem(806,-104169.05,-97108.82,'Bom','Sem Abrigo','No','Vimeca',[12],308,'Estrada do Desvio','Carnaxide e Queijas').
paragem(687,-102942.61,-98628.76,'Bom','Fechado dos Lados','Yes','Vimeca',[12],830,'Estrada Militar','Barcarena').
paragem(686,-102931.23,-98622.69,'Bom','Sem Abrigo','No','Vimeca',[12],830,'Estrada Militar','Barcarena').
paragem(155,-102735.06,-98272.9,'Mau','Fechado dos Lados','No','Vimeca',[12],830,'Estrada Militar','Barcarena').
paragem(154,-103016.79,-98428.89,'Bom','Fechado dos Lados','Yes','Vimeca',[12],830,'Estrada Militar','Barcarena').
paragem(87,-103002.83,-98398.75,'Bom','Aberto dos Lados','No','Vimeca',[12],830,'Estrada Militar','Barcarena').
paragem(149,-102638.72,-98781.31,'Bom','Sem Abrigo','No','Vimeca',[12],993,'Rua do Trabalho','Barcarena').
paragem(159,-102708.54,-98296.07,'Bom','Sem Abrigo','No','Vimeca',[12],1099,'Rua Quinta da Bica do Sargento','Barcarena').
paragem(709,-103166.65231804183,-97987.56576748956,'Bom','Sem Abrigo','No','Vimeca',[12],1200,'Rua Actor Carlos Cesar','Carnaxide e Queijas').
paragem(1014,-103181.82,-97967.06,'Bom','Sem Abrigo','No','Vimeca',[12],1214,'Estrada de Queluz','Carnaxide e Queijas').
paragem(788,-103468.05,-97872.21,'Bom','Fechado dos Lados','Yes','Vimeca',[12],1214,'Estrada de Queluz','Carnaxide e Queijas').
paragem(742,-102859.54,-97965.24,'Bom','Fechado dos Lados','Yes','Vimeca',[12],1214,'Estrada de Queluz','Carnaxide e Queijas').
paragem(741,-102845.12,-97961.08,'Bom','Sem Abrigo','No','Vimeca',[12],1214,'Estrada de Queluz','Carnaxide e Queijas').
paragem(170,-103478.11,-97851.67,'Bom','Sem Abrigo','No','Vimeca',[12],1214,'Estrada de Queluz','Carnaxide e Queijas').
paragem(68,-103193.0811132861,-97956.29135098259,'Bom','Sem Abrigo','No','Vimeca',[12],1214,'Estrada de Queluz','Carnaxide e Queijas').
paragem(128,-101966.52,-98573.78,'Bom','Fechado dos Lados','Yes','Vimeca',[12,02,06,13],216,'Estrada Consiglieri Pedroso','Barcarena').
paragem(212,-104337.69,-101982.34,'Bom','Sem Abrigo','No','LT',[122],1631,'Rua Sete de Junho','Porto Salvo').
paragem(201,-104367.8,-102011.46,'Bom','Sem Abrigo','No','LT',[122],1631,'Rua Sete de Junho','Porto Salvo').
paragem(746,-103402.21,-102780,'Bom','Sem Abrigo','No','LT',[122],1652,'Avenida Diogo Lopes de Sequeira','Porto Salvo').
paragem(165,-103427.95,-102788.2,'Bom','Fechado dos Lados','No','LT',[122],1652,'Avenida Diogo Lopes de Sequeira','Porto Salvo').
paragem(164,-103464.95,-102647.48,'Bom','Fechado dos Lados','No','LT',[122],1652,'Avenida Diogo Lopes de Sequeira','Porto Salvo').
paragem(163,-103452.97,-102640.79,'Bom','Fechado dos Lados','No','LT',[122],1652,'Avenida Diogo Lopes de Sequeira','Porto Salvo').
paragem(747,-103418.71,-102966.92,'Bom','Fechado dos Lados','No','Carris',[122],1653,'Avenida Gaspar Corte Real','Porto Salvo').
paragem(198,-104273.72,-102318.75,'Bom','Sem Abrigo','No','LT',[122,125,129,184],695,'Avenida Dom Pedro V','Porto Salvo').
paragem(207,-104333.51,-102157.13,'Bom','Aberto dos Lados','No','LT',[122,125,129,184],705,'Avenida Engenheiro Arantes e Oliveira','Porto Salvo').
paragem(199,-104309.46,-102333.17,'Bom','Fechado dos Lados','No','LT',[122,125,184],705,'Avenida Engenheiro Arantes e Oliveira','Porto Salvo').
paragem(761,-103589.75,-102328.21,'Bom','Sem Abrigo','No','LT',[122,125,184],1651,'Avenida Lopo Soares de Albergaria','Porto Salvo').
paragem(760,-103595.2,-102342.08,'Bom','Fechado dos Lados','No','LT',[122,125,184],1651,'Avenida Lopo Soares de Albergaria','Porto Salvo').
paragem(759,-103754.86,-102383.62,'Bom','Sem Abrigo','No','LT',[122,125,184],1692,'Rua Augusta','Porto Salvo').
paragem(758,-103782.6,-102354.54,'Bom','Fechado dos Lados','No','LT',[122,125,184],1692,'Rua Augusta','Porto Salvo').
paragem(204,-104054.48,-102333.36,'Bom','Fechado dos Lados','No','LT',[122,125,184],1692,'Rua Augusta','Porto Salvo').
paragem(203,-104060.31,-102343.26,'Bom','Sem Abrigo','No','LT',[122,125,184],1692,'Rua Augusta','Porto Salvo').
paragem(166,-104013.89,-102412.58,'Bom','Sem Abrigo','No','LT',[122,125,184],1698,'Rua de Sao Jose','Porto Salvo').
paragem(197,-104911.17,-102075.08,'Bom','Sem Abrigo','No','LT',[122,129],687,'Rua do Casal do Deserto','Porto Salvo').
paragem(196,-104948.14,-102024.5,'Bom','Sem Abrigo','No','LT',[122,129],687,'Rua do Casal do Deserto','Porto Salvo').
paragem(269,-105203.57,-102507.02,'Bom','Sem Abrigo','No','LT',[122,129],755,'Rua Oliveira Martins','Porto Salvo').
paragem(268,-105213.17,-102489.26,'Bom','Fechado dos Lados','Yes','LT',[122,129],755,'Rua Oliveira Martins','Porto Salvo').
paragem(189,-104980,-102444.39,'Bom','Sem Abrigo','No','LT',[122,129],755,'Rua Oliveira Martins','Porto Salvo').
paragem(188,-104998.59,-102447.95,'Bom','Sem Abrigo','No','LT',[122,129],755,'Rua Oliveira Martins','Porto Salvo').
paragem(211,-104613.4385006046,-102059.62741233375,'Bom','Sem Abrigo','No','LT',[122,129],1611,'Rua das Portelas','Porto Salvo').
paragem(202,-104559.73,-102074.01,'Bom','Fechado dos Lados','No','LT',[122,129],1611,'Rua das Portelas','Porto Salvo').
paragem(195,-104888.12,-101925.58,'Bom','Sem Abrigo','No','LT',[122,129],1611,'Rua das Portelas','Porto Salvo').
paragem(194,-104887.93,-101935.17,'Bom','Sem Abrigo','No','LT',[122,129],1611,'Rua das Portelas','Porto Salvo').
paragem(200,-104420.26,-102140.16,'Bom','Sem Abrigo','No','LT',[122,129],1631,'Rua Sete de Junho','Porto Salvo').
paragem(206,-104352.88,-102155.61,'Bom','Fechado dos Lados','No','LT',[122,129,184],690,'Rua do Comercio','Porto Salvo').
paragem(440,-106999.56,-100744.24,'Bom','Fechado dos Lados','Yes','LT',[125,129,158,184],605,'Rua Conde das Alcacovas','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(210,-104329.45,-101849.83,'Bom','Sem Abrigo','No','LT',[125,129,184],705,'Avenida Engenheiro Arantes e Oliveira','Porto Salvo').
paragem(209,-104318.74,-101876.43,'Bom','Aberto dos Lados','No','LT',[125,129,184],705,'Avenida Engenheiro Arantes e Oliveira','Porto Salvo').
paragem(753,-103040.03,-102067.93,'Bom','Fechado dos Lados','Yes','LT',[125,184],1634,'Estrada de Talai-de','Porto Salvo').
paragem(730,-102764.70414054944,-102345.36371072767,'Bom','Sem Abrigo','No','LT',[125,184],1634,'Estrada de Talai-de','Porto Salvo').
paragem(721,-103007.51,-102085.97,'Bom','Fechado dos Lados','Yes','LT',[125,184],1634,'Estrada de Talai-de','Porto Salvo').
paragem(133,-102770.03,-102362.19,'Bom','Fechado dos Lados','No','LT',[125,184],1634,'Estrada de Talai-de','Porto Salvo').
paragem(558,-107118.74,-101197.15,'Bom','Fechado dos Lados','No','LT',[129,115,125,158],1527,'Rua Manuel Pinhancos','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(279,-105866.72,-100896.59,'Bom','Fechado dos Lados','Yes','LT',[129,184],858,'Rua Calvet de Magalhaes','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(205,-104431.06,-101723.48,'Bom','Fechado dos Lados','Yes','Vimeca',[15],692,'Rua Conde de Rio Maior','Porto Salvo').
paragem(193,-104911.86,-101688.38,'Bom','Fechado dos Lados','Yes','Vimeca',[15],692,'Rua Conde de Rio Maior','Porto Salvo').
paragem(192,-104730.80639856319,-101677.18184016421,'Bom','Sem Abrigo','No','Vimeca',[15],692,'Rua Conde de Rio Maior','Porto Salvo').
paragem(190,-104741.4,-101691.52,'Bom','Fechado dos Lados','Yes','SCoTTURB',[15],692,'Rua Conde de Rio Maior','Porto Salvo').
paragem(798,-104277.99,-101693.69,'Bom','Fechado dos Lados','Yes','Vimeca',[15],735,'Estrada de Leiao','Porto Salvo').
paragem(763,-103544.73,-101579.29,'Bom','Fechado dos Lados','No','Vimeca',[15],735,'Estrada de Leiao','Porto Salvo').
paragem(754,-103586.35,-101579.63,'Bom','Fechado dos Lados','Yes','Vimeca',[15],735,'Estrada de Leiao','Porto Salvo').
paragem(314,-105206.62,-98321.51,'Bom','Fechado dos Lados','Yes','Vimeca',[15],830,'Estrada Militar','Carnaxide e Queijas').
paragem(280,-105520.95,-101295.9,'Bom','Fechado dos Lados','Yes','Vimeca',[15],1488,'Avenida Conselho da Europa','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(278,-105488.63,-101308.47,'Bom','Fechado dos Lados','Yes','Vimeca',[15],1488,'Avenida Conselho da Europa','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(286,-105046.86,-101627.86,'Bom','Fechado dos Lados','Yes','SCoTTURB',[15],1540,'Estrada de Paco de Arcos','Porto Salvo').
paragem(728,-102509.72,-101859.8,'Bom','Fechado dos Lados','Yes','Vimeca',[15],1667,'Avenida Professor Dr. Cavaco Silva','Porto Salvo').
paragem(724,-102849.51,-101421.76,'Bom','Fechado dos Lados','Yes','Vimeca',[15],1667,'Avenida Professor Dr. Cavaco Silva','Porto Salvo').
paragem(270,-105268.41,-102428.49,'Bom','Fechado dos Lados','Yes','Vimeca',[15],1680,'Rua Encosta das Lagoas','Porto Salvo').
paragem(295,-105236.25,-102190.54,'Bom','Fechado dos Lados','Yes','Vimeca',[15],1682,'Avenida Santa Casa da Misericordia de Oeiras','Porto Salvo').
paragem(293,-105274.84,-101913.18,'Bom','Fechado dos Lados','Yes','Vimeca',[15],1682,'Avenida Santa Casa da Misericordia de Oeiras','Porto Salvo').
paragem(292,-105227.47,-102176.58,'Bom','Fechado dos Lados','Yes','Vimeca',[15],1682,'Avenida Santa Casa da Misericordia de Oeiras','Porto Salvo').
paragem(291,-105291.98,-101912.29,'Bom','Fechado dos Lados','Yes','Vimeca',[15],1682,'Avenida Santa Casa da Misericordia de Oeiras','Porto Salvo').
paragem(719,-102227.22,-101894.71,'Bom','Fechado dos Lados','Yes','Vimeca',[15],1667,'Avenida Professor Dr. Cavaco Silva','Porto Salvo').
paragem(729,-102515.87,-101878.09,'Bom','Fechado dos Lados','Yes','Vimeca',[15,23],1667,'Avenida Professor Dr. Cavaco Silva','Porto Salvo').
paragem(718,-102227.55,-101920.36,'Bom','Fechado dos Lados','Yes','Vimeca',[15,23],1667,'Avenida Professor Dr. Cavaco Silva','Porto Salvo').
paragem(706,-101856.51,-101822.02,'Bom','Fechado dos Lados','Yes','Vimeca',[15,23],1667,'Avenida Professor Dr. Cavaco Silva','Porto Salvo').
paragem(703,-101884.93,-101826.65,'Bom','Fechado dos Lados','Yes','Vimeca',[15,23],1667,'Avenida Professor Dr. Cavaco Silva','Porto Salvo').
paragem(129,-102865.58,-101399.39,'Bom','Fechado dos Lados','Yes','Vimeca',[15,23],1667,'Avenida Professor Dr. Cavaco Silva','Porto Salvo').
paragem(406,-106251.97,-101287.62,'Bom','Fechado dos Lados','Yes','LT',[158],637,'Avenida dos Fundadores','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(405,-106237.68,-101291.27,'Bom','Fechado dos Lados','Yes','LT',[158],637,'Avenida dos Fundadores','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(400,-106447.18,-101426.26,'Bom','Fechado dos Lados','Yes','LT',[158],637,'Avenida dos Fundadores','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(397,-106091.14,-101154.18,'Bom','Fechado dos Lados','Yes','LT',[158],637,'Avenida dos Fundadores','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(396,-106081.08,-101165.77,'Bom','Fechado dos Lados','Yes','LT',[158],637,'Avenida dos Fundadores','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(395,-106446.17,-101412.04,'Bom','Fechado dos Lados','Yes','LT',[158],637,'Avenida dos Fundadores','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(296,-105981.14,-99626.06,'Bom','Sem Abrigo','No','LT',[158],850,'Rua Antonio Pires','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(443,-106400.89,-99913.2,'Bom','Aberto dos Lados','No','LT',[158],853,'Rua Bartolomeu Dias','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(305,-105957.65,-99532.97,'Bom','Sem Abrigo','No','LT',[158],858,'Rua Calvet de Magalhaes','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(304,-105812.82,-99920.11,'Bom','Sem Abrigo','No','LT',[158],858,'Rua Calvet de Magalhaes','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(303,-105819.47,-99953.55,'Bom','Sem Abrigo','No','LT',[158],858,'Rua Calvet de Magalhaes','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(433,-106203.75,-99942.88,'Bom','Aberto dos Lados','No','LT',[158],863,'Rua dos Cedros','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(884,-106100.40608156208,-99324.93367751369,'Bom','Sem Abrigo','No','LT',[158],871,'Rua Dom Francisco de Almeida','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(298,_,_,'Bom','Aberto dos Lados','No','LT',[158],890,'Rua Fernando Vaz','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(297,-105936.26,-99903.99,'Bom','Aberto dos Lados','No','LT',[158],890,'Rua Fernando Vaz','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(410,-106940.98,-99253.35,'Bom','Fechado dos Lados','No','LT',[158],898,'Estrada da Gibalta','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(275,-106970.5,-99227.97,'Bom','Fechado dos Lados','Yes','LT',[158],898,'Estrada da Gibalta','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(316,-105973.13,-98916.25,'Bom','Fechado dos Lados','Yes','LT',[158],925,'Estrada do Murganhal','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(308,-105439.02,-99406,'Bom','Fechado dos Lados','No','LT',[158],932,'Rua da Pedreira Italiana','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(307,-105549.91,-99320.86,'Bom','Sem Abrigo','No','LT',[158],932,'Rua da Pedreira Italiana','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(434,-106212.36,-99846.22,'Bom','Aberto dos Lados','No','LT',[158],933,'Rua Pero de Alenquer','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(430,-106489.64,-99992.66,'Bom','Aberto dos Lados','No','LT',[158],933,'Rua Pero de Alenquer','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(219,-104719.91,-99745.14,'Bom','Sem Abrigo','No','LT',[158],1013,'Rua Quinta da Moura','Barcarena').
paragem(221,-104939.44,-99815.59,'Bom','Sem Abrigo','No','LT',[158],1014,'Rua do Castelo','Barcarena').
paragem(220,-104914,-99807.09,'Bom','Sem Abrigo','No','LT',[158],1014,'Rua do Castelo','Barcarena').
paragem(301,-105137.41,-99828.18,'Bom','Sem Abrigo','No','LT',[158],1018,'Rua do Alto da Peca','Barcarena').
paragem(300,-105118.79,-99818.36,'Bom','Sem Abrigo','No','LT',[158],1018,'Rua do Alto da Peca','Barcarena').
paragem(826,-105726.06,-102732.3,'Bom','Fechado dos Lados','Yes','LT',[158],1521,'Estrada da Ribeira da Laje','Porto Salvo').
paragem(1005,-105735.17290016402,-100578.1564002752,'Bom','Sem Abrigo','No','LT',[158],1578,'Rua Manuel Viegas Guerreiro','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(306,-105860.41,-100520.47,'Bom','Sem Abrigo','No','LT',[158],1578,'Rua Manuel Viegas Guerreiro','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(302,-105785.16,-100273.12,'Bom','Sem Abrigo','No','LT',[158],1585,'Avenida Antonio Sena da Silva','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(833,-105686.35894762371,-100239.52088707738,'Bom','Sem Abrigo','No','LT',[158],1605,'Avenida Professor Antonio Maria Baptista Fernandes','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(438,-106599.44,-99556.41,'Bom','Aberto dos Lados','No','LT',[158],1769,'Rua Vasco da Gama','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(311,-105814.63,-99290.4,'Bom','Fechado dos Lados','Yes','LT',[158],1801,'Rua Viscondessa de Santo Amaro','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(669,-106112.38652897865,-95027.7101712073,'Bom','Fechado dos Lados','Yes','LT',[162],10,'Avenida dos Bombeiros Voluntarios de Alges','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(678,-106071.42513405527,-95039.14634930693,'Bom','Fechado dos Lados','Yes','LT',[162],10,'Avenida dos Bombeiros Voluntarios de Alges','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(674,-106560.62,-95186.03,'Bom','Fechado dos Lados','Yes','LT',[162],10,'Avenida dos Bombeiros Voluntarios de Alges','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(668,-106342.5,-95131.58,'Bom','Fechado dos Lados','Yes','LT',[162],10,'Avenida dos Bombeiros Voluntarios de Alges','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(76,-106288.85,-95136.57,'Bom','Fechado dos Lados','Yes','LT',[162],10,'Avenida dos Bombeiros Voluntarios de Alges','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(73,-106568.5,-95165.9,'Bom','Fechado dos Lados','Yes','LT',[162],10,'Avenida dos Bombeiros Voluntarios de Alges','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(528,-107089.71,-95214.56,'Bom','Fechado dos Lados','Yes','LT',[162],102,'Largo Dom Manuel I','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(639,-105456.01,-94993.65,'Bom','Fechado dos Lados','Yes','LT',[162],116,'Avenida General Norton de Matos','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(636,-105462.27,-94976.17,'Bom','Fechado dos Lados','Yes','LT',[162],116,'Avenida General Norton de Matos','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(634,-105696.83,-95075.27,'Bom','Fechado dos Lados','Yes','LT',[162],116,'Avenida General Norton de Matos','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(12,-105655.76,-95028.52,'Bom','Fechado dos Lados','Yes','LT',[162],116,'Avenida General Norton de Matos','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(666,-106799.63,-95251.22,'Bom','Sem Abrigo','No','LT',[162],155,'Praca Doutor Manuel Martins','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(658,-106786.85846811837,-95149.7421827531,'Bom','Fechado dos Lados','Yes','LT',[162],155,'Praca Doutor Manuel Martins','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(630,-104458.04,-94329.86,'Bom','Fechado dos Lados','No','LT',[162],1123,'Rua da Quinta do Paizinho','Carnaxide e Queijas').
paragem(629,-104278.88666597521,-94122.56603635015,'Bom','Sem Abrigo','No','LT',[162],1123,'Rua da Quinta do Paizinho','Carnaxide e Queijas').
paragem(626,-104287.85,-94105.37,'Bom','Fechado dos Lados','Yes','LT',[162],1123,'Rua da Quinta do Paizinho','Carnaxide e Queijas').
paragem(616,-104497.842173306,-94358.908881103,'Bom','Fechado dos Lados','Yes','LT',[162],1123,'Rua da Quinta do Paizinho','Carnaxide e Queijas').
paragem(228,-104460.75,-98562.29,'Bom','Sem Abrigo','No','LT',[171],83,'Rua Angra do Heroismo','Carnaxide e Queijas').
paragem(105,-101764.82,-99761.18,'Bom','Fechado dos Lados','Yes','LT',[171],261,'Rua da Juventude','Barcarena').
paragem(104,-101753.46,-99755.19,'Bom','Fechado dos Lados','Yes','LT',[171],261,'Rua da Juventude','Barcarena').
paragem(225,-104591.62,-98511.89,'Bom','Sem Abrigo','No','LT',[171],846,'Rua da Quinta do Bonfim','Carnaxide e Queijas').
paragem(1012,-101927.83891266519,-99709.84354381096,'Bom','Sem Abrigo','No','LT',[171],1006,'Rua Antonio Quadros','Barcarena').
paragem(115,-101877.84,-99707.56,'Bom','Sem Abrigo','No','LT',[171],1006,'Rua Antonio Quadros','Barcarena').
paragem(765,-103522.68,-99425.21,'Bom','Fechado dos Lados','Yes','LT',[171,117],230,'Rua Elias Garcia','Barcarena').
paragem(764,-103545.91,-99424.63,'Bom','Sem Abrigo','No','LT',[171,117],230,'Rua Elias Garcia','Barcarena').
paragem(110,-104942.33,-101650.59,'Bom','Sem Abrigo','No','LT',[184],697,'Avenida dos Descobrimentos','Porto Salvo').
paragem(113,-104747.63,-101297.99,'Bom','Sem Abrigo','No','LT',[184],703,'Rua Doutor Jose Filipe Rodrigues','Porto Salvo').
paragem(112,-104759.55,-101277.77,'Bom','Sem Abrigo','No','LT',[184],703,'Rua Doutor Jose Filipe Rodrigues','Porto Salvo').
paragem(111,-104852.21,-101412.86,'Bom','Sem Abrigo','No','LT',[184],703,'Rua Doutor Jose Filipe Rodrigues','Porto Salvo').
paragem(114,-104842.95,-101406.66,'Bom','Sem Abrigo','No','LT',[184],756,'Pateo das Padeiras','Porto Salvo').
paragem(633,-105696.83,-95075.27,'Bom','Fechado dos Lados','Yes','Carris',[201,748,750,751],116,'Avenida General Norton de Matos','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(15,-105325.87,-95135.44,'Bom','Fechado dos Lados','Yes','Carris',[201,748,751],113,'Alameda FerNo Lopes','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(54,-105456.01,-94993.65,'Bom','Fechado dos Lados','Yes','Carris',[201,748,750,751],116,'Avenida General Norton de Matos','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(645,-105353.27,-95172.19,'Bom','Fechado dos Lados','Yes','Carris',[201,748,751],113,'Alameda FerNo Lopes','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(651,-105300.44,-95336.46,'Bom','Fechado dos Lados','Yes','Carris',[201,748,751],113,'Alameda FerNo Lopes','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(650,-105259.11583333602,-95350.71833333441,'Bom','Sem Abrigo','No','Carris',[201,748,751],113,'Alameda FerNo Lopes','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(13,-105268.05,-95547.68,'Bom','Fechado dos Lados','Yes','Carris',[201,748,751],124,'Avenida Jose Gomes Ferreira','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(23,-105261.03,-95520.31,'Bom','Sem Abrigo','No','Carris',[201,748,751],124,'Avenida Jose Gomes Ferreira','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(11,-105158.82133137222,-95894.13861202101,'Bom','Fechado dos Lados','Yes','Carris',[201,748,751],416,'Alameda Antonio Sergio','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(673,-106563.02096789006,-95186.78384945756,'Bom','Sem Abrigo','No','Carris',[201,750,751],10,'Avenida dos Bombeiros Voluntarios de Alges','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(670,-106112.38652897863,-95027.71017120728,'Bom','Fechado dos Lados','Yes','Carris',[201,750,751],10,'Avenida dos Bombeiros Voluntarios de Alges','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(677,-106071.42513405527,-95039.14634930693,'Bom','Fechado dos Lados','Yes','Carris',[201,750,751],10,'Avenida dos Bombeiros Voluntarios de Alges','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(676,-106283.09180093784,-95136.51301607292,'Bom','Sem Abrigo','Yes','Carris',[201,750,751],10,'Avenida dos Bombeiros Voluntarios de Alges','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(667,-106342.5,-95131.58,'Bom','Fechado dos Lados','Yes','Carris',[201,750,751],10,'Avenida dos Bombeiros Voluntarios de Alges','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(656,-106786.85846811837,-95149.7421827531,'Bom','Fechado dos Lados','Yes','Carris',[201,750,751],155,'Praca Doutor Manuel Martins','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(69,-106799.79,-95252.14,'Bom','Sem Abrigo','No','Carris',[201,750,751],155,'Praca Doutor Manuel Martins','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(640,-105655.76,-95028.52,'Bom','Fechado dos Lados','Yes','Carris',[201,751],116,'Avenida General Norton de Matos','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(53,-105462.27,-94976.17,'Bom','Fechado dos Lados','Yes','Carris',[201,751],116,'Avenida General Norton de Matos','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(100,-102002.37,-102008.48,'Bom','Fechado dos Lados','No','Vimeca',[23],1668,'Avenida Engenheiro Valente de Oliveira','Porto Salvo').
paragem(720,-103014.4,-101951.36,'Bom','Fechado dos Lados','No','Vimeca',[23],1670,'Avenida Domingos Vandelli','Porto Salvo').
paragem(131,-102556.6,-102172.39,'Bom','Sem Abrigo','No','Vimeca',[23],1671,'Rua Professor Dr. Jose Pinto Peixoto','Porto Salvo').
paragem(393,-106368.26,-101705.73,'Bom','Sem Abrigo','Yes','Vimeca',[30],587,'Avenida Antonio Bernardo Cabral de Macedo','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(923,-107625.08,-103117.77,'Bom','Fechado dos Lados','No','Vimeca',[30],1431,'Avenida Salvador Allende','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(921,-107096.82640151314,-103853.54646127204,'Bom','Sem Abrigo','No','SCoTTURB',[467],1404,'Rua Norton de Matos','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(523,-107058.08,-103860.82,'Bom','Sem Abrigo','No','SCoTTURB',[467],1404,'Rua Norton de Matos','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(513,-107854.63,-102915.29,'Bom','Fechado dos Lados','Yes','SCoTTURB',[467,468,470,479,485,489],1422,'Rua da Quinta Grande','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(509,-107387.31,-103679.6,'Bom','Fechado dos Lados','Yes','SCoTTURB',[467,468,479],1426,'Avenida da Republica','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(508,-107491.16,-103120.89,'Bom','Fechado dos Lados','Yes','SCoTTURB',[467,468,479],1426,'Avenida da Republica','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(507,-107368.48,-103668.54,'Bom','Fechado dos Lados','No','SCoTTURB',[467,468,479],1426,'Avenida da Republica','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(924,-107625.08,-103117.77,'Bom','Fechado dos Lados','No','SCoTTURB',[467,468,479,471],1431,'Avenida Salvador Allende','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(495,-107142.69,-103759.12,'Bom','Fechado dos Lados','Yes','SCoTTURB',[467,479],1421,'Rotunda da Quinta do Marques','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(263,-107314.88,-104013.15,'Bom','Fechado dos Lados','Yes','SCoTTURB',[468,470,485,489],1426,'Avenida da Republica','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(262,-107284.78,-104045.09,'Bom','Fechado dos Lados','Yes','SCoTTURB',[468,470,485,489],1426,'Avenida da Republica','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(372,-107025.86,-103844.74,'Bom','Sem Abrigo','No','SCoTTURB',[470,479,485,489],1338,'Avenida Goncalves Zarco',_).
paragem(267,-105726.06,-102732.3,'Bom','Fechado dos Lados','Yes','SCoTTURB',[470,485],1521,'Estrada da Ribeira da Laje','Porto Salvo').
paragem(927,-107720.8493590646,-103624.00113664303,'Bom','Sem Abrigo','No','SCoTTURB',[470,485,489],550,'Alameda Conde de Oeiras','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(515,-107839.61,-103572.1,'Bom','Sem Abrigo','No','SCoTTURB',[470,485,489],550,'Alameda Conde de Oeiras','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(520,-107795.85,-103878.54,'Bom','Fechado dos Lados','Yes','SCoTTURB',[470,485,489],557,'Avenida Dom Jose I','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(519,-107802.03,-103891.09,'Bom','Sem Abrigo','No','SCoTTURB',[470,485,489],557,'Avenida Dom Jose I','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(265,-107430.38,-103996.06,'Bom','Fechado dos Lados','Yes','SCoTTURB',[470,485,489],557,'Avenida Dom Jose I','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(264,-107670.49,-103999.05,'Bom','Fechado dos Lados','Yes','SCoTTURB',[470,485,489],557,'Avenida Dom Jose I','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(512,-107836.26,-103714.18,'Bom','Fechado dos Lados','Yes','SCoTTURB',[470,485,489],569,'Rua Doutor Jose Carlos Moreira','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(511,-107822.87,-103711.43,'Bom','Sem Abrigo','No','SCoTTURB',[470,485,489],569,'Rua Doutor Jose Carlos Moreira','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(556,-107825.00067489177,-103153.47411185557,'Bom','Sem Abrigo','No','SCoTTURB',[470,485,489],1422,'Rua da Quinta Grande','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(522,-107811.57,-103173.61,'Bom','Fechado dos Lados','Yes','SCoTTURB',[470,485,489],1422,'Rua da Quinta Grande','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(500,-107558.36,-103601.65,'Bom','Fechado dos Lados','Yes','SCoTTURB',[470,485,489],1516,'Rua Monsenhor Ferreira de Melo','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(825,-107055.50456594216,-104067.91249783144,'Bom','Sem Abrigo','No','SCoTTURB',[470,485,489,467,475,479],1338,'Avenida Goncalves Zarco',_).
paragem(824,-107062.58,-104020.28,'Bom','Fechado dos Lados','No','SCoTTURB',[470,485,489,467,479],1338,'Avenida Goncalves Zarco',_).
paragem(587,-108937.83,-103208.76,'Razoavel','Fechado dos Lados','Yes','SCoTTURB',[471],491,'Rua de Aljubarrota','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(834,-107559.62,-102708.32,'Bom','Fechado dos Lados','Yes','SCoTTURB',[471],514,'Largo Aviao Lusitania','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(960,-107102.8,-101781.42,'Bom','Fechado dos Lados','Yes','SCoTTURB',[471],527,'Avenida de Brasilia','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(933,-107581.42,-102198.59,'Bom','Fechado dos Lados','Yes','SCoTTURB',[471],533,'Rua Candido dos Reis','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(506,-107655.98,-102504.64,'Bom','Fechado dos Lados','Yes','SCoTTURB',[471],533,'Rua Candido dos Reis','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(974,-108611,-103212.55,'Bom','Fechado dos Lados','Yes','SCoTTURB',[471],556,'Avenida Dom Joao I','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(971,-108633.94,-103087.73,'Bom','Fechado dos Lados','Yes','SCoTTURB',[471],556,'Avenida Dom Joao I','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(918,-107752.93,-102745.45,'Bom','Fechado dos Lados','Yes','SCoTTURB',[471],559,'Rua Desembargador Faria','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(939,-108680.61,-103239.46,'Razoavel','Fechado dos Lados','Yes','SCoTTURB',[471],561,'Rua Dona Filipa de Lencastre','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(502,-107319.63,-102563.55,'Bom','Fechado dos Lados','Yes','SCoTTURB',[471],578,'Avenida Embaixador Augusto de Castro','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(537,-107286.31,-102095.09,'Bom','Fechado dos Lados','No','SCoTTURB',[471],1325,'Rua da Figueirinha','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(574,-108145.87,-103052.15,'Bom','Fechado dos Lados','Yes','SCoTTURB',[471],1332,'Rua da Fundicao de Oeiras','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(1026,-108103.16416368041,-103073.41174351703,'Bom','Sem Abrigo','No','SCoTTURB',[471],1342,'Rua Henrique de Paiva Couceiro','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(580,-108654.89,-103440.08,'Bom','Fechado dos Lados','Yes','SCoTTURB',[471],1344,'Avenida Infante Dom Henrique','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(497,-108401.93,-103222.84,'Bom','Sem Abrigo','No','SCoTTURB',[471],1344,'Avenida Infante Dom Henrique','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(914,-108221.94694854727,-102975.10717631762,'Bom','Aberto dos Lados','No','SCoTTURB',[471],1346,'Rua Infanta Dona Isabel','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(575,-108450.51,-102954.49,'Bom','Fechado dos Lados','Yes','SCoTTURB',[471],1346,'Rua Infanta Dona Isabel','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(928,-107653.99,-103018.24,'Bom','Fechado dos Lados','Yes','SCoTTURB',[471],1372,'Rua dos Lagares da Quinta','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(560,-107381,-101739.33,'Bom','Fechado dos Lados','Yes','SCoTTURB',[471],1398,'Avenida de Mocambique','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(559,-107472.33,-101878.29,'Bom','Fechado dos Lados','Yes','SCoTTURB',[471],1398,'Avenida de Mocambique','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(561,-107268.49,-101728.6,'Bom','Fechado dos Lados','Yes','SCoTTURB',[471],1440,'Rua Sao Salvador da Baia','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(376,-107047.8,-103631.28,'Bom','Sem Abrigo','No','SCoTTURB',[479],1315,'Rua das Escolas','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(375,-107044.63,-103620.23,'Bom','Fechado dos Lados','Yes','SCoTTURB',[479],1315,'Rua das Escolas','Oeiras e Sao Juliao da Barra, Paco de Arcos e Caxias').
paragem(980,-104256.82,-95173.34,'Bom','Fechado dos Lados','Yes','Carris',[714],306,'Rua dos Cravos de Abril','Carnaxide e Queijas').
paragem(685,-104174.54200948933,-95114.07850277536,'Bom','Sem Abrigo','No','Carris',[714],347,'Rua da Liberdade','Carnaxide e Queijas').
paragem(603,-104172.6851196953,-95216.43740152338,'Bom','Sem Abrigo','No','Carris',[714],347,'Rua da Liberdade','Carnaxide e Queijas').
paragem(623,-104578.88,-94652.12,'Bom','Sem Abrigo','No','Carris',[714],365,'Estrada da Portela','Carnaxide e Queijas').
paragem(1032,-104222.84172433561,-94001.25535769734,'Bom','Fechado dos Lados','Yes','Carris',[714],1123,'Rua da Quinta do Paizinho','Carnaxide e Queijas').
paragem(631,-104458.04,-94329.86,'Bom','Fechado dos Lados','No','Carris',[714],1123,'Rua da Quinta do Paizinho','Carnaxide e Queijas').
paragem(627,-104278.88666597521,-94122.56603635015,'Bom','Sem Abrigo','No','Carris',[714],1123,'Rua da Quinta do Paizinho','Carnaxide e Queijas').
paragem(615,-104497.842173306,-94358.908881103,'Bom','Fechado dos Lados','Yes','Carris',[714],1123,'Rua da Quinta do Paizinho','Carnaxide e Queijas').
paragem(619,-104458.52,-94926.22,'Bom','Fechado dos Lados','Yes','Carris',[714],1134,'Largo Sete de Junho de 1759','Carnaxide e Queijas').
paragem(43,-104445.64,-94921.33,'Bom','Fechado dos Lados','No','Carris',[714],1134,'Largo Sete de Junho de 1759','Carnaxide e Queijas').
paragem(979,-104677.06,-94473.47,'Bom','Fechado dos Lados','No','Carris',[714],1160,'Rua Cincinato da Costa','Carnaxide e Queijas').
paragem(978,-104683.1,-94486.15,'Bom','Fechado dos Lados','No','Carris',[714],1160,'Rua Cincinato da Costa','Carnaxide e Queijas').
paragem(14,-105367.42,-95012.5,'Bom','Sem Abrigo','No','Carris',[748],113,'Alameda FerNo Lopes','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(21,-105136.75,-95897.19,'Bom','Fechado dos Lados','Yes','Carris',[748,751],416,'Alameda Antonio Sergio','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(456,-107086.94,-95183.62,'Bom','Fechado dos Lados','Yes','Carris',[750],102,'Largo Dom Manuel I',_).
paragem(672,-106566.19596789329,-95165.08801610209,'Bom','Sem Abrigo','No','Carris',[750,751,201],10,'Avenida dos Bombeiros Voluntarios de Alges','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(1031,-107014.28376646667,-95156.66564817408,'Bom','Sem Abrigo','No','Carris',[751, 201],102,'Largo Dom Manuel I',_).
paragem(168,-107095.22,-95206.35,'Bom','Fechado dos Lados','Yes','Carris',[751, 201],102,'Largo Dom Manuel I','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(144,-106979.51,-95226.45,'Bom','Fechado dos Lados','Yes','Carris',[776],103,'Rua Damiao de Gois','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(794,-106975.22,-95602.61,'Bom','Sem Abrigo','No','Carris',[776],118,'Alameda Hermano Patrone','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(681,-107008.56,-95490.23,'Bom','Fechado dos Lados','No','Carris',[776],118,'Alameda Hermano Patrone','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(187,-106985.92,-95598.8,'Bom','Sem Abrigo','No','Carris',[776],118,'Alameda Hermano Patrone','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(481,-106532.67,-97275.79,'Bom','Sem Abrigo','No','Carris',[776],367,'Estrada da Costa','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(459,-106331.99,-97379.59,'Bom','Fechado dos Lados','Yes','Carris',[776],367,'Estrada da Costa','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(897,-107004.52,-96080.98,'Bom','Fechado dos Lados','No','Carris',[776],369,'Rua Direita do Dafundo','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(895,-106999.08,-96066.1,'Bom','Fechado dos Lados','No','Carris',[776],369,'Rua Direita do Dafundo','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(493,-106898.93,-96325.82,'Bom','Fechado dos Lados','No','Carris',[776],369,'Rua Direita do Dafundo','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(471,-106865.6,-96906.59,'Bom','Sem Abrigo','No','Carris',[776],386,'Rua Sacadura Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(906,-106791.2,-97137.51,'Bom','Fechado dos Lados','Yes','Carris',[776],386,'Rua Sacadura Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(484,-106790.95,-97111.1,'Bom','Sem Abrigo','No','Carris',[776],386,'Rua Sacadura Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(479,-106688.65,-97277.31,'Bom','Sem Abrigo','No','Carris',[776],386,'Rua Sacadura Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(478,-106680.98,-97288.83,'Bom','Fechado dos Lados','Yes','Carris',[776],386,'Rua Sacadura Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(477,-106835.46,-96672.9,'Bom','Fechado dos Lados','Yes','Carris',[776],386,'Rua Sacadura Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(475,-106826.16,-96699.81,'Bom','Sem Abrigo','No','Carris',[776],386,'Rua Sacadura Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(474,-106880.09,-96852.94,'Bom','Sem Abrigo','No','Carris',[776],386,'Rua Sacadura Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(463,-106886.32,-96345.37,'Bom','Sem Abrigo','No','Carris',[776],386,'Rua Sacadura Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(8,-106980.35,-95289.3,'Bom','Sem Abrigo','No','Carris',[776],103,'Rua Damiao de Gois','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(5,-106997.31,-95311.49,'Bom','Sem Abrigo','No','Carris',[776],103,'Rua Damiao de Gois','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo').
paragem(167,-107073,-95199.03,'Bom','Fechado dos Lados','Yes','Carris',[98],102,'Largo Dom Manuel I',_).
