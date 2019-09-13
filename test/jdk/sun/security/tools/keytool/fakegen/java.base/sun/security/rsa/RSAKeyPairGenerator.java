/*
 * Copyright (c) 2019, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package sun.security.rsa;

import java.math.BigInteger;

import java.security.*;
import java.security.spec.AlgorithmParameterSpec;
import java.security.spec.RSAKeyGenParameterSpec;

import sun.security.x509.AlgorithmId;
import static sun.security.rsa.RSAUtil.KeyType;

/**
 * A fake RSA keypair generation.
 */
public abstract class RSAKeyPairGenerator extends KeyPairGeneratorSpi {

    // public exponent to use
    private BigInteger publicExponent;

    // size of the key to generate, >= RSAKeyFactory.MIN_MODLEN
    private int keySize;

    private final KeyType type;
    private AlgorithmId rsaId;

    RSAKeyPairGenerator(KeyType type, int defKeySize) {
        this.type = type;
        // initialize to default in case the app does not call initialize()
        initialize(defKeySize, null);
    }

    // initialize the generator. See JCA doc
    public void initialize(int keySize, SecureRandom random) {
        try {
            initialize(new RSAKeyGenParameterSpec(keySize,
                    RSAKeyGenParameterSpec.F4), random);
        } catch (InvalidAlgorithmParameterException iape) {
            throw new InvalidParameterException(iape.getMessage());
        }
    }

    // second initialize method. See JCA doc.
    public void initialize(AlgorithmParameterSpec params, SecureRandom random)
            throws InvalidAlgorithmParameterException {
        if (params instanceof RSAKeyGenParameterSpec == false) {
            throw new InvalidAlgorithmParameterException
                ("Params must be instance of RSAKeyGenParameterSpec");
        }

        RSAKeyGenParameterSpec rsaSpec = (RSAKeyGenParameterSpec)params;
        int tmpKeySize = rsaSpec.getKeysize();
        BigInteger tmpPublicExponent = rsaSpec.getPublicExponent();
        AlgorithmParameterSpec tmpParams = rsaSpec.getKeyParams();

        if (tmpPublicExponent == null) {
            tmpPublicExponent = RSAKeyGenParameterSpec.F4;
        } else {
            if (tmpPublicExponent.compareTo(RSAKeyGenParameterSpec.F0) < 0) {
                throw new InvalidAlgorithmParameterException
                        ("Public exponent must be 3 or larger");
            }
            if (tmpPublicExponent.bitLength() > tmpKeySize) {
                throw new InvalidAlgorithmParameterException
                        ("Public exponent must be smaller than key size");
            }
        }

        // do not allow unreasonably large key sizes, probably user error
        try {
            RSAKeyFactory.checkKeyLengths(tmpKeySize, tmpPublicExponent,
                512, 64 * 1024);
        } catch (InvalidKeyException e) {
            throw new InvalidAlgorithmParameterException(
                "Invalid key sizes", e);
        }

        try {
            this.rsaId = RSAUtil.createAlgorithmId(type, tmpParams);
        } catch (ProviderException e) {
            throw new InvalidAlgorithmParameterException(
                "Invalid key parameters", e);
        }

        this.keySize = tmpKeySize;
        this.publicExponent = tmpPublicExponent;
    }

    // generate the keypair. See JCA doc
    public KeyPair generateKeyPair() {

        // accommodate odd key sizes in case anybody wants to use them
        BigInteger e = publicExponent;
        if (!e.equals(RSAKeyGenParameterSpec.F4)) {
            throw new AssertionError("Only support F4 now");
        }
        BigInteger p, q, n;

        // Pre-calculated p and q for e == RSAKeyGenParameterSpec.F4
        switch (keySize) {
            case 1024:
                p = new BigInteger("1220491537800192366196661816910427"
                        + "2375185130493819649338056226264568132442590"
                        + "2306195110391300298681932797393339089272174"
                        + "24490645345596103420476757417659909");

                q = new BigInteger("8480533592205316739308384508307319"
                        + "3310632635895778398980504245932789486455154"
                        + "4265220956952343855842030266079089174423047"
                        + "7382175514060777025691485728713063");
                break;
            case 2048:
                p = new BigInteger("1600840041787354447543653385760927"
                        + "2642568308955833364523274045522752644800599"
                        + "8669541532595690224703734511692014533312515"
                        + "1867029838883431415692353449578487671384896"
                        + "6611685764860941767986520897595108597563035"
                        + "4023785639802607792535812062420427283857665"
                        + "9883578590844700707106157871508280052743363"
                        + "65749456332400771");
                q = new BigInteger("1303880717101677622201474394769850"
                        + "7257196073324816341282215626935164930077468"
                        + "5999131251387556761167658937349436378464220"
                        + "4831804147777472146628148336776639855791417"
                        + "3849903041999943901924899580268176393595653"
                        + "7357080543898614581363167420619163047562600"
                        + "6155574020606891195960345238780709194499010"
                        + "43652862954645301");
                break;
            case 3072:
                p = new BigInteger("2403380417344527161525447148950543"
                        + "9379802436047502603774623852967909282895900"
                        + "7474251859703715384817150107392592636129366"
                        + "5680725148417789414447073716354703692160825"
                        + "9910929198523923851672957013786423746474670"
                        + "5285365870313877239114930859096040034848729"
                        + "0251735848703378313724697081522892607625120"
                        + "0765531003751468146539682665307123591367928"
                        + "1883284784295069877414057796300484015307336"
                        + "5167008365209621810307777651197826810474895"
                        + "858836606799546054179898997210174885200767");
                q = new BigInteger("2046511893459993309772203642844931"
                        + "1853027469856467052942666265372903000702193"
                        + "7794121141028918655144044411085405243565188"
                        + "8318027937964509940322691121105328104450287"
                        + "0400405975394764523022670537180050051646095"
                        + "3320242109876620452477757629185501504625999"
                        + "8487187901577781067325277853462587849063313"
                        + "5364789330253471096755661110557598411108366"
                        + "1566266965711522070909440716658568303529454"
                        + "8075296935907088988560548322049196321207173"
                        + "136436923455192617031129191723631954025427");
                break;
            case 3073:
                p = new BigInteger("358024230418365680745725628188289"
                        + "996969857993064124357766607077169315932503"
                        + "907030633492082868150575329278637502167157"
                        + "345572819552678004302525872656665704012432"
                        + "644868036964802792468216554458209893873320"
                        + "297608885231998895441396822219845215463819"
                        + "216163684222596923153702349718019538798623"
                        + "793830598445085650304217449539016339429747"
                        + "385490484982419227549804698120223647408926"
                        + "021549912791440569127641517442362359166673"
                        + "430151753277549861070074076802118983202554"
                        + "7683523973");
                q = new BigInteger("169752169194244078720180277210205"
                        + "255207796420396114596943944148158798629671"
                        + "789863988428383183103705641694331073747120"
                        + "522505020908219489265190380712941311249155"
                        + "156834674079165492105570443486536740756291"
                        + "314279877766807786397193023304245520017295"
                        + "969248707516367324340822062886043618371887"
                        + "320492710434958863952407959935025922006108"
                        + "282752473898685457767312430894004833959025"
                        + "415996648289919841900071076314309778495245"
                        + "505026665971431792707247787031811654193145"
                        + "1044952887");
                break;
            case 4096:
                p = new BigInteger("2985635754414679487171962796211911"
                        + "1563710734938215274736352092606404045130913"
                        + "2477365484439939846705721840432140066578525"
                        + "0762327458086280430118434094733412377416194"
                        + "8736124795243564050755767519346747209606612"
                        + "5835460937739428885308798309679495432910469"
                        + "0294757621321446003970767164933974474924664"
                        + "1513767092845098947552598109657871041666676"
                        + "2945573325433283821164032766425479703026349"
                        + "9433641551427112483593214628620450175257586"
                        + "4350119143877183562692754400346175237007314"
                        + "7121580349193179272551363894896336921717843"
                        + "3734726842184251708799134654802475890197293"
                        + "9094908310578403843742664173424031260840446"
                        + "591633359364559754200663");
                q = new BigInteger("2279248439141087793789384816271625"
                        + "1304008816573950275844533962181244003563987"
                        + "6638461665174020058827698592331066726709304"
                        + "9231319346136709972639455506783245161859951"
                        + "6191872757335765533547033659834427437142631"
                        + "3801232751161907082392011429712327250253948"
                        + "6012497852063361866175243227579880020724881"
                        + "9393797645220239009219998518884396282407710"
                        + "7199202450846395844337846503427790307364624"
                        + "5124871273035872938616425951596065309519651"
                        + "1519189356431513094684173807318945903212527"
                        + "7712469749366620048658571121822171067675915"
                        + "5479178304648399924549334007222294762969503"
                        + "5341584429803583589276956979963609078497238"
                        + "760757619468018224491053");
                break;
            case 7680:
                p = new BigInteger("7034022146817764608206409206476311"
                        + "1371065855827199565170055133179419153145313"
                        + "9446295819321510144417300286482767498463255"
                        + "3370362723164324606829434617977090251035572"
                        + "4237546099249090966627372485629853471350368"
                        + "1497807044971675189990783927066958945388379"
                        + "4004871857862380913954692362042250112646330"
                        + "0515873693830575810241740671573357342073942"
                        + "4924331206670599086552994426505996651481869"
                        + "2750320309695596383830444337180596058381417"
                        + "7804256675894755482917598033151085120879826"
                        + "5373459707672000040090469058320463160804122"
                        + "8041026671361647347262771363754358441620179"
                        + "3861076191970047581401830740749715862725492"
                        + "5750077182986169592435996668282677192000592"
                        + "8019204141383012670399208092972174321639234"
                        + "6398779487064860458178135981700487728919191"
                        + "2418625085287791733119321378648688730352488"
                        + "6446800847442687530322927871063574241918202"
                        + "1883228799435533794167861499482410970370569"
                        + "6964781912514810444018313637829915454156777"
                        + "4591735979781496237582756842195362757823524"
                        + "2054367976655738752756266147542536910268553"
                        + "7448833810249238249431673425245843356607647"
                        + "4372260685985882252621076435844355190011754"
                        + "0656637395317040751098009821385816878380790"
                        + "9017022563392158510700243843871343995665725"
                        + "9447");
                q = new BigInteger("7006097449483280289139842436346899"
                        + "9530483972793167261845287040200424598484519"
                        + "0516644904497480849788319004016685820778949"
                        + "8514891188272714344395538114013074460410497"
                        + "8359325044727263936454825245761418873141623"
                        + "2797767712619624879346520836103128457266811"
                        + "0175147598232556414901292143389437071453369"
                        + "4867355570712858957689566849950544938172174"
                        + "9136345403774445991224311311204082279382451"
                        + "1977673518543939329498617563049052032259503"
                        + "1057378839061411322975567618416093845144604"
                        + "4563500271329350225709899627798126176885843"
                        + "9953695524756659384424971651654225913916902"
                        + "7087953078966988666226779422822854729725410"
                        + "1118666318953891716194309185187152294057704"
                        + "2509582485250984938518224243061447485603856"
                        + "0197517312286511585545481984194518805766300"
                        + "7351141154720653351520788558277866920369211"
                        + "9269193267018615410395030712426882576851775"
                        + "9240726416572698143674385203349496226355950"
                        + "0327319442530888807531774734409411628435155"
                        + "6449625936156829175453919098709371623972260"
                        + "4090598351878256110255744263233419699486705"
                        + "3658219936031243191809689036438631617494147"
                        + "8183898954195399872571907981872765548994005"
                        + "0754996819683869837696561316479950526914796"
                        + "3613549619563212818369365543951734053135086"
                        + "8593");
                break;
            case 7681:
                p = new BigInteger("1051422012172516922972337796421710"
                        + "1479918158427421377774284533124281583092508"
                        + "4961153980854679659383249048667658521370629"
                        + "6404078819943386006431233653506112030196761"
                        + "7596078830067404622117784094070983115834842"
                        + "0487926993058767148294211654032287322495894"
                        + "1733666341539644761909820700670130074920230"
                        + "0423405253327351685101039824822997870642295"
                        + "8176417666723292317903368285439828460328413"
                        + "9578515412811253729224087502906626189200448"
                        + "4062549581079444644685394008569377879899377"
                        + "0578136407363616816108795867392262638913436"
                        + "2041457188733948013731661456602199370852579"
                        + "2394583215214266552642082043674873685065171"
                        + "9053703728689139119006448644148961706358756"
                        + "0498696516029596534138345596800696824976461"
                        + "8461070371773003241093580622731426623849732"
                        + "8737707046931934781383331589291927520571138"
                        + "2759511573966362005294438252298268405782746"
                        + "9642105075721912120520774388679358659061825"
                        + "8867415333830751976884165265610029632416351"
                        + "6666642762305875140340531095190000199307531"
                        + "2186952558457690950270489966695323839026041"
                        + "0797018634946454573060304991245539422899112"
                        + "9312288231966690989900334936215870198735213"
                        + "6745631923445509394027128331099748294658904"
                        + "6303809606943116678969915369410846798143779"
                        + "89187");
                q = new BigInteger("8160183444544784072886152354850963"
                        + "2507221530634202707531181684048014518183560"
                        + "4586261698204521358487995639975786964316103"
                        + "5861955444259138806900123878887948650444640"
                        + "7062365744302695074634248387572850855758081"
                        + "9074631976865727985289995811413472254074432"
                        + "4751574514329862821405069035713821908598155"
                        + "5436571566703295992717666421591959121774983"
                        + "5913798632992394695663301960139778059658979"
                        + "4944466598070019481108421780314115410201334"
                        + "2925668957166258484312039121420100795544341"
                        + "5372918969907063685116069321551182817324799"
                        + "3347159610063964281388144113123539915925090"
                        + "2692309096312451442405059120315891913678403"
                        + "4977738880336169371406947347468157606390086"
                        + "3381587148478097717352225924672548657441771"
                        + "3887371520215341151934095945869956390140929"
                        + "3098286281540595154062683214111934217839063"
                        + "3309526631019699109621050440794920159910038"
                        + "3248965999877529393614116991972153758910967"
                        + "1712258745878268303349611893651932564447696"
                        + "9601760120187828039923387985032881949408596"
                        + "0689784023540256749586591441103043888423326"
                        + "3078541569639917739590630101701573133092711"
                        + "6157430583592378472242178997833136144409256"
                        + "3558220277370179546822023437190315852529873"
                        + "0831890147323301322699740037060134979740963"
                        + "4289");
                break;
            case 8192:
                p = new BigInteger("9821669838446774374944535804569858"
                        + "0553278885576950130485823829973470553571905"
                        + "3014418421996241500307589880457361653957913"
                        + "9176499436767288125182942994089196450118944"
                        + "8701794862752733776161684616570463744619126"
                        + "4981622564763630694110472008409561205704867"
                        + "0221819623405201369630462487520858670679048"
                        + "5854008441429858453634949980424333056803703"
                        + "1205609490778445762604050796894221725977551"
                        + "1428887194691696420765173256600200430067305"
                        + "4364524177041858044598166859757042904625691"
                        + "4292728453597609683799189454690202563236931"
                        + "8171122071288244573793276051041975005528757"
                        + "0228306442708182141334279133965507583927772"
                        + "9244311696220253059281524393613278272067808"
                        + "7017494446447670799055720358621918361716353"
                        + "5018317015764698318012095108914870478138809"
                        + "8204738169777192718869484177321870413838036"
                        + "8149216482968887382371881239714335470844573"
                        + "1862934371951394070111726593305334971041399"
                        + "5517260339034138718517336990212463882142363"
                        + "9154412320743552301967162100734381046548816"
                        + "3883737645359595416600487444018399886391071"
                        + "3777667222706059170707223589163679915863781"
                        + "4662302526078720977228426750718207481384357"
                        + "7918717041190413457052439016978578217755022"
                        + "7370720979516554707297685239584071755267452"
                        + "6021894842754355160100506065457679069228273"
                        + "95209345267367982516553449135291473361");
                q = new BigInteger("7902448465953646210110784092684896"
                        + "0265474424590294110174550047938700740921014"
                        + "1981650823416127449143596912363210790070524"
                        + "2903784112701128957948996730263815210531364"
                        + "0489145287401377007608600217628773627723381"
                        + "1194123533939872283952535576847014977682278"
                        + "9332064706645169741712060131540562788886577"
                        + "3762235020990267901959745687867018811088495"
                        + "3716021011509120447248882358515954471433808"
                        + "2782236662758287959413069553620728137831579"
                        + "2321174813204514354999978428741310035945405"
                        + "0226661395731921098764192439072425262100813"
                        + "9732949866553839713092238096261034339815187"
                        + "2832617055364163276140160068136296115910569"
                        + "9466440903693740716929166334256441926903849"
                        + "1082968246155177124035336609654226388424434"
                        + "5775783323612758615407928446164631651292743"
                        + "8428509642959278732826297890909454571009075"
                        + "7836191622138731918099379467912681177757761"
                        + "6141378131042432093843778753846726589215845"
                        + "7402160146427434508515156204064224022904659"
                        + "8645441448874409852211668374267341177082462"
                        + "7341410218867175406105046487057429530801973"
                        + "0931082058719258230993681115780999537424968"
                        + "2385515792331573549935317407789344892257264"
                        + "7464569110078675090194686816764429827739815"
                        + "0566036514181547634372488184242167294602000"
                        + "8232780963578241583529875079397308150506597"
                        + "37190564909892937290776929541076192569");
                break;
            default:
                throw new AssertionError("Unknown keySize " + keySize);
        }

        n = p.multiply(q);

        // phi = (p - 1) * (q - 1) must be relative prime to e
        // otherwise RSA just won't work ;-)
        BigInteger p1 = p.subtract(BigInteger.ONE);
        BigInteger q1 = q.subtract(BigInteger.ONE);
        BigInteger phi = p1.multiply(q1);
        // generate new p and q until they work. typically
        // the first try will succeed when using F4
        if (e.gcd(phi).equals(BigInteger.ONE) == false) {
            throw new AssertionError("Should not happen");
        }

        // private exponent d is the inverse of e mod phi
        BigInteger d = e.modInverse(phi);

        // 1st prime exponent pe = d mod (p - 1)
        BigInteger pe = d.mod(p1);
        // 2nd prime exponent qe = d mod (q - 1)
        BigInteger qe = d.mod(q1);

        // crt coefficient coeff is the inverse of q mod p
        BigInteger coeff = q.modInverse(p);

        try {
            PublicKey publicKey = new RSAPublicKeyImpl(rsaId, n, e);
            PrivateKey privateKey = new RSAPrivateCrtKeyImpl(
                    rsaId, n, e, d, p, q, pe, qe, coeff);
            return new KeyPair(publicKey, privateKey);
        } catch (InvalidKeyException exc) {
            // invalid key exception only thrown for keys < 512 bit,
            // will not happen here
            throw new RuntimeException(exc);
        }
    }
}
